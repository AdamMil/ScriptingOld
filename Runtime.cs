/*
Scripting is a low level framework for building dynamic languages.
It produces languages which can be interpreted or compiled, targetting
the Microsoft .NET Framework.

http://www.adammil.net/
Copyright (C) 2005-2006 Adam Milazzo

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Globalization;
using System.Reflection;
using System.Reflection.Emit;
using Scripting.Backend;

// FIXME: implement tail-call elimination in interpreted mode
namespace Scripting
{

#region Interfaces
public interface IProcedure
{ int MinArgs { get; }
  int MaxArgs { get; }
  bool NeedsFreshArgs { get; }

  object Call(params object[] args);
}

public interface IFancyProcedure : IProcedure
{ object Call(object[] positional, string[] keywords, object[] values);
}

public interface IProperty
{ bool Call(object instance, out object ret, params object[] args);
  bool Call(object instance, out object ret, object[] positional, string[] names, object[] values);
  bool Get(object instance, out object ret);
  bool GetAccessor(object instance, out IProcedure ret);
  bool TrySet(object instance, object value);
}
#endregion

#region Binding
public sealed class Binding
{ public Binding(string name, MemberContainer from) { Value=Unbound; Name=name; From=from; }
  public Binding(string name, object value, MemberContainer from) { Value=value; Name=name; From=from; }

  public override bool Equals(object obj) { return this==obj; }
  public override int GetHashCode() { return Name.GetHashCode(); }

  public object Value;
  public string Name;
  public MemberContainer From;

  public readonly static object Unbound = new Singleton("<UNBOUND>");
}
#endregion

#region Environments
#region BindingSpace
public sealed class BindingSpace
{ public void Alter(string name, object value)
  { Binding bind;
    lock(Dict) if(!Dict.TryGetValue(name, out bind)) throw UndefinedVariableException.FromName(name);
    bind.Value = value;
  }

  public void Bind(string name, object value, MemberContainer from)
  { Binding bind;
    lock(Dict)
      if(!Dict.TryGetValue(name, out bind) || bind.From!=from) Dict[name] = bind = new Binding(name, from);
    bind.Value = value;
  }

  public bool Contains(string name)
  { Binding bind;
    lock(Dict) return Dict.TryGetValue(name, out bind) && bind.Value!=Binding.Unbound;
  }

  public object Get(string name)
  { Binding bind;
    lock(Dict)
      if(!Dict.TryGetValue(name, out bind) || bind.Value==Binding.Unbound)
        throw UndefinedVariableException.FromName(name);
    return bind.Value;
  }

  public bool Get(string name, out object value)
  { Binding bind;
    lock(Dict) if(!Dict.TryGetValue(name, out bind) || bind.Value==Binding.Unbound) { value=null; return false; }
    value = bind.Value;
    return true;
  }

  public Binding GetBinding(string name, MemberContainer from)
  { Binding bind;
    lock(Dict) if(!Dict.TryGetValue(name, out bind)) Dict[name] = bind = new Binding(name, from);
    return bind;
  }

  public void Set(string name, object value, MemberContainer from)
  { Binding bind;
    lock(Dict)
      if(Dict.TryGetValue(name, out bind)) bind.From = from;
      else Dict[name] = bind = new Binding(name, from);
    bind.Value = value;
  }

  public void Unbind(string name) { lock(Dict) Dict.Remove(name); }

  public readonly Dictionary<string,Binding> Dict = new Dictionary<string,Binding>();
}
#endregion

#region InterpreterEnvironment
public sealed class InterpreterEnvironment
{ public InterpreterEnvironment(InterpreterEnvironment parent) { this.parent=parent; dict=new ListDictionary(); }

  public void Bind(string name, object value) { dict[name]=value; }

  public object Get(string name)
  { object ret = dict[name];
    if(ret==null && !dict.Contains(name)) return parent!=null ? parent.Get(name) : TopLevel.Current.Get(name);
    return ret;
  }

  public void Set(string name, object value)
  { if(dict.Contains(name)) dict[name] = value;
    else if(parent!=null) parent.Set(name, value);
    else TopLevel.Current.Set(name, value);
  }

  InterpreterEnvironment parent;
  ListDictionary dict;

  [ThreadStatic] public static InterpreterEnvironment Current;
}
#endregion

#region LocalEnvironment
public sealed class LocalEnvironment
{ public LocalEnvironment(LocalEnvironment parent, object[] values) { Parent=parent; Values=values; }
  public LocalEnvironment(LocalEnvironment parent, int length) { Parent=parent; Values=new object[length]; }
  public LocalEnvironment(LocalEnvironment parent, int length, object[] values)
  { Parent=parent; Values=new object[length]; values.CopyTo(Values, 0);
  }

  public readonly LocalEnvironment Parent;
  public readonly object[] Values;
}
#endregion

#region TopLevel
public sealed class TopLevel : MemberContainer
{ 
  #region MemberContainer
  public override ICollection<string> GetMemberNames(bool includeImports)
  { if(includeImports) return Globals.Dict.Keys;
    else
    { List<string> list = new List<string>();
      foreach(KeyValuePair<string,Binding> de in Globals.Dict) if(de.Value.From==this) list.Add(de.Key);
      return list;
    }
  }

  public override bool GetSlot(object instance, string name, out object ret) { return Globals.Get(name, out ret); }

  public override void Export(TopLevel top, string[] names, string[] asNames)
  { ExportInfo ei = new ExportInfo(this);

    if(names==null)
    { foreach(KeyValuePair<string,Binding> de in Globals.Dict)
      { string exportName = ei.GetExternalName(de.Key);
        if(exportName!=null) top.Globals.Bind(exportName, de.Value.Value, de.Value.From);
      }
      if(Macros!=null)
      { if(top.Macros==null) top.Macros = new BindingSpace();
        foreach(KeyValuePair<string,Binding> de in Macros.Dict)
        { string exportName = ei.GetExternalName(de.Key);
          if(exportName!=null) top.Macros.Bind(exportName, de.Value.Value, de.Value.From);
        }
      }
    }
    else
    { if(asNames==null) asNames = names;

      for(int i=0; i<names.Length; i++)
      { string name = ei.GetInternalName(names[i]);
        Binding bind;
        bool found = false;
        if(name!=null)
        { if(Macros.Dict.TryGetValue(name, out bind))
          { top.Macros.Bind(asNames[i], bind.Value, bind.From);
            found = true;
          }
          if(Globals.Dict.TryGetValue(name, out bind))
          { top.Globals.Bind(asNames[i], bind.Value, bind.From);
            found = true;
          }
        }
        if(!found) throw new ArgumentException("TopLevel does not contain a member called '"+name+
                                               (name!=names[i] ? "' (renamed from '"+names[i]+"')" : "'"));
      }
    }
  }

  public override bool TryDeleteSlot(object instance, string name)
  { Globals.Unbind(name);
    return true;
  }

  public override bool TrySetSlot(object instance, string name, object value)
  { Globals.Set(name, value, this);
    return true;
  }
  #endregion

  public void AddMacro(string name, IProcedure value)
  { if(Macros==null) Macros = new BindingSpace();
    Macros.Set(name, value, this);
  }

  public void Alter(string name, object value) { Globals.Alter(name, value); }
  public void Bind(string name, object value) { Globals.Bind(name, value, this); }

  public new bool Contains(string name) { return Globals.Contains(name); }
  public bool ContainsMacro(string name) { return Macros!=null && Macros.Contains(name); }

  public object Get(string name) { return Globals.Get(name); }
  public bool Get(string name, out object value) { return Globals.Get(name, out value); }
  public Binding GetBinding(string name) { return Globals.GetBinding(name, this); }

  public IProcedure GetMacro(string name)
  { if(Macros==null) throw UndefinedVariableException.FromName(name);
    return (IProcedure)Macros.Get(name);
  }

  public void Set(string name, object value) { Globals.Set(name, value, this); }
  public void Unbind(string name) { Globals.Unbind(name); }

  public BindingSpace Globals=new BindingSpace(), Macros;

  [ThreadStatic] public static TopLevel Current;
}
#endregion
#endregion

#region MemberContainer
public abstract class MemberContainer
{ 
  #region ExportInfo
  public sealed class ExportInfo
  { public ExportInfo() { noMapping = true; }

    public ExportInfo(TopLevel top) : this(top, null, null) { }
    public ExportInfo(TopLevel top, string[] names) : this(top, names, null) { }
    public ExportInfo(TopLevel top, string[] names, string[] asNames)
    { if(names==null)
      { map = new Dictionary<string,string>();
        foreach(KeyValuePair<string,Binding> de in top.Globals.Dict) if(de.Value.From==top) map[de.Key] = de.Key;
        if(top.Macros!=null)
          foreach(KeyValuePair<string,Binding> de in top.Macros.Dict) if(de.Value.From==top) map[de.Key] = de.Key;
        noMapping = true;
      }
      else
      { map = new Dictionary<string,string>(names.Length);
        if(asNames==null) { noMapping=true; asNames=names; }
        for(int i=0; i<names.Length; i++)
        { if(!top.Globals.Contains(names[i]) && (top.Macros==null || !top.Macros.Contains(names[i])))
            throw new ArgumentException("Export name '"+names[i]+"' not found");
          map[names[i]] = asNames[i];
        }
      }
    }

    public ExportInfo(MemberContainer mc) : this(mc, null, null) { }
    public ExportInfo(MemberContainer mc, string[] names) : this(mc, names, null) { }
    public ExportInfo(MemberContainer mc, string[] names, string[] asNames)
    { if(names==null)
      { ICollection<string> mcNames = mc.GetMemberNames(false);
        map = new Dictionary<string,string>(mcNames.Count);
        foreach(string name in mcNames) map[name] = name;
        noMapping = true;
      }
      else
      { if(asNames==null) { noMapping=true; asNames=names; }
        map = new Dictionary<string,string>(names.Length);
        for(int i=0; i<names.Length; i++)
        { if(!mc.Contains(names[i])) throw new ArgumentException("Export name '"+names[i]+"' not found");
          map[names[i]] = asNames[i];
        }
      }
    }

    public string GetInternalName(string name)
    { if(map==null) return name[0]=='_' ? null : name;
      if(noMapping) return map.ContainsKey(name) ? name : null;
      if(reverseMap==null)
      { reverseMap = new Dictionary<string,string>(map.Count);
        foreach(KeyValuePair<string,string> de in map) reverseMap[de.Value] = de.Key;
      }
      reverseMap.TryGetValue(name, out name);
      return name;
    }

    public string GetExternalName(string name)
    { return map==null ? name[0]=='_' ? null : name : map.TryGetValue(name, out name) ? name : null;
    }

    public bool GetExternalName(string name, out string exportName)
    { if(map!=null) return map.TryGetValue(name, out exportName);
      else if(name[0]=='_') { exportName=null; return false; }
      else { exportName=name; return true; }
    }
    
    public static readonly ExportInfo Simple = new ExportInfo();

    Dictionary<string,string> map, reverseMap;
    bool noMapping;
  }
  #endregion

  #region SlotAccessor
  public sealed class SlotAccessor : IProcedure
  { public SlotAccessor(string member) { Member = member; }

    public int MinArgs { get { return 1; } }
    public int MaxArgs { get { return 2; } }
    public bool NeedsFreshArgs { get { return false; } }

    public object Call(object[] args)
    { if(args.Length==1) return Ops.GetSlot(args[0], Member);
      else if(args.Length==2) { Ops.SetSlot(args[1], args[0], Member); return null; }
      else throw Ops.ArityError("slot accessor", args.Length, 1, 2);
    }

    public readonly string Member;
  }
  #endregion

  public bool Contains(string name)
  { object dummy;
    return GetSlot(null, name, out dummy);
  }

  public void DeleteSlot(object instance, string name)
  { if(!TryDeleteSlot(instance, name))
      throw new AttributeException(instance, name,
                                   string.Format("can't delete attribute '{0}' on '{1}'", name, Ops.TypeName(instance)));
  }
  public abstract bool TryDeleteSlot(object instance, string name);

  public void Export(TopLevel top) { Export(top, null, null); }
  public void Export(TopLevel top, string[] names) { Export(top, names, names); }
  public abstract void Export(TopLevel top, string[] names, string[] asNames);

  public ICollection<string> GetMemberNames() { return GetMemberNames(false); }
  public abstract ICollection<string> GetMemberNames(bool includeImports);

  public object GetSlot(object instance, string name)
  { object ret;
    if(!GetSlot(instance, name, out ret))
      throw new AttributeException(instance, name,
                                   string.Format("can't get attribute '{0}' on '{1}'", name, Ops.TypeName(instance)));
    return ret;
  }

  public abstract bool GetSlot(object instance, string name, out object ret);

  public void SetSlot(object instance, string name, object value)
  { if(!TrySetSlot(instance, name, value))
      throw new AttributeException(instance, name,
                                   string.Format("can't set attribute '{0}' on '{1}'", name, Ops.TypeName(instance)));
  }

  public abstract bool TrySetSlot(object instance, string name, object value);

  public IProcedure GetAccessor(object instance, string name)
  { IProcedure ret;
    if(!GetAccessor(instance, name, out ret))
      throw new AttributeException(instance, name,
                                   string.Format("can't get accessor '{0}' on '{1}'", name, Ops.TypeName(instance)));
    return ret;
  }

  public bool GetAccessor(object instance, string name, out IProcedure ret)
  { object slot;
    if(!GetSlot(instance, name, out slot)) { ret=null; return false; }
    IProperty prop = slot as IProperty;
    if(prop!=null) return prop.GetAccessor(instance, out ret);
    else
    { ret = new SlotAccessor(name);
      return true;
    }
  }

  public object CallProperty(object instance, string name, params object[] args)
  { object ret;
    if(!CallProperty(instance, name, out ret, args))
      throw new AttributeException(instance, name,
                                   string.Format("can't call property '{0}' on '{1}'", name, Ops.TypeName(instance)));
    return ret;
  }

  public bool CallProperty(object instance, string name, out object ret, params object[] args)
  { if(!GetSlot(instance, name, out ret)) return false;
    IProperty prop = ret as IProperty;
    if(prop!=null) return prop.Call(instance, out ret, args);
    else
    { IProcedure proc = Ops.MakeProcedure(ret);
      if(proc==null) { ret=null; return false; }
      else
      { // FIXME: this (and the other stuff like it in DotNetInterop.cs) is inefficient, and is exactly what i was trying to get away from with this new system...
        object[] nargs = new object[args.Length-1];
        Array.Copy(args, 1, nargs, 0, nargs.Length);
        ret = proc.Call(nargs);
        return true;
      }
    }
  }

  public object CallProperty(object instance, string name, object[] positional, string[] names, object[] values)
  { object ret;
    if(!CallProperty(instance, name, out ret, positional, names, values))
      throw new AttributeException(instance, name,
                                   string.Format("can't call property '{0}' on '{1}'", name, Ops.TypeName(instance)));
    return ret;
  }

  public bool CallProperty(object instance, string name, out object ret,
                           object[] positional, string[] names, object[] values)
  { if(!GetSlot(instance, name, out ret)) return false;
    IProperty prop = ret as IProperty;
    if(prop!=null) return prop.Call(instance, out ret, positional, names, values);
    else
    { IFancyProcedure proc = ret as IFancyProcedure;
      if(proc==null) { ret=null; return false; }
      else { ret=proc.Call(positional, names, values); return true; }
    }
  }

  public object GetProperty(object instance, string name)
  { object ret;
    if(!GetProperty(instance, name, out ret))
      throw new AttributeException(instance, name,
                                   string.Format("can't get property '{0}' on '{1}'", name, Ops.TypeName(instance)));
    return ret;
  }

  public bool GetProperty(object instance, string name, out object ret)
  { if(!GetSlot(instance, name, out ret)) return false;
    IProperty prop = ret as IProperty;
    return prop==null ? true : prop.Get(instance, out ret);
  }

  public void SetProperty(object instance, string name, object value)
  { if(!TrySetProperty(instance, name, value))
      throw new AttributeException(instance, name,
                                   string.Format("can't set property '{0}' on '{1}'", name, Ops.TypeName(instance)));
  }

  public bool TrySetProperty(object instance, string name, object value)
  { object slot;
    if(GetSlot(instance, name, out slot))
    { IProperty prop = slot as IProperty;
      if(prop!=null) return prop.TrySet(instance, value);
    }
    return TrySetSlot(instance, name, value);
  }

  public static MemberContainer FromObject(object obj)
  { MemberContainer mc = obj as MemberContainer;
    return mc!=null ? mc : obj==null ? ReflectedType.NullType : ReflectedType.FromType(obj.GetType());
  }
}
#endregion

#region MultipleValues
public sealed class MultipleValues
{ public MultipleValues(params object[] values) { Values=values; }

  public override string ToString()
  { System.Text.StringBuilder sb = new System.Text.StringBuilder();
    sb.Append('{');
    bool sep=false;
    for(int i=0; i<Values.Length; i++)
    { if(sep) sb.Append(", ");
      else sep=true;
      sb.Append(Ops.ToCode(Values[i]));
    }
    return sb.Append('}').ToString();
  }

  public object[] Values;
}
#endregion

#region Ops
public static class Ops
{ 
  #region Errors
  public static ArgumentException ArgError(string message) { return new ArgumentException(message); }
  public static ArgumentException ArgError(string format, params object[] args)
  { return new ArgumentException(string.Format(format, args));
  }

  public static ArgumentException ArityError(string name, int nargs, int min, int max)
  { if(max==-1)
    { if(nargs<min) return new ArgumentException(name+": expects at least "+min.ToString()+
                                                 " arguments, but received "+nargs.ToString());
    }
    else if(nargs<min || nargs>max)
      return new ArgumentException(name+": expects "+(min==max ? min.ToString() : min.ToString()+"-"+max.ToString())+
                                   " arguments, but received "+nargs.ToString());
    return new ArgumentException("wrong number of arguments");
  }

  public static void CheckArity(string name, object[] args, int num) { CheckArity(name, args.Length, num, num); }
  public static void CheckArity(string name, object[] args, int min, int max)
  { CheckArity(name, args.Length, min, max);
  }
  public static void CheckArity(string name, int nargs, int min, int max)
  { if(max==-1)
    { if(nargs<min) throw new ArgumentException(name+": expects at least "+min.ToString()+
                                                " arguments, but received "+nargs.ToString());
    }
    else if(nargs<min || nargs>max)
      throw new ArgumentException(name+": expects "+(min==max ? min.ToString() : min.ToString()+"-"+max.ToString())+
                                  " arguments, but received "+nargs.ToString());
  }

  public static void CheckType(string name, object[] args, int index, Type type)
  { if(Node.AreEquivalent(args[index]==null ? null : args[index].GetType(), type)) return;
    try
    { if(type==typeof(int)) { ExpectInt(args[index]); return; }
    }
    catch { }
    throw new ArgumentException(string.Format("{0}: for argument {1}, expects type {2} but received {3}",
                                              name, index, TypeName(type), TypeName(args[index])));
  }

  public static Binding CheckBinding(Binding bind)
  { if(bind.Value==Binding.Unbound) throw UndefinedVariableException.FromName(bind.Name);
    return bind;
  }

  public static void CheckVariable(object value, string name)
  { if(value==Binding.Unbound) throw UndefinedVariableException.FromName(name);
  }

  public static object[] CheckValues(MultipleValues values, int length)
  { if(values.Values.Length<length)
      throw new ArgumentException("expected at least "+length.ToString()+" values, but received "+
                                  values.Values.Length.ToString());
    return values.Values;
  }

  public static char ExpectChar(object obj)
  { try { return (char)obj; }
    catch(InvalidCastException) { throw new ArgumentException("expected character but received "+TypeName(obj)); }
  }

  // TODO: coerce numbers into complexes?
  public static Complex ExpectComplex(object obj)
  { try { return (Complex)obj; }
    catch(InvalidCastException) { throw new ArgumentException("expected complex but received "+TypeName(obj)); }
  }

  public static IEnumerator ExpectEnumerator(object obj)
  { IEnumerable ea = obj as IEnumerable;
    IEnumerator  e = ea==null ? obj as IEnumerator : ea.GetEnumerator();
    if(e==null) throw new ArgumentException("expected enumerable object but received "+Ops.TypeName(obj));
    return e;
  }

  public static IFancyProcedure ExpectFancyProcedure(object obj)
  { IFancyProcedure ret = obj as IFancyProcedure;
    if(ret==null) throw new ArgumentException("expected fancy function but received "+TypeName(obj));
    return ret;
  }

  public static int ExpectInt(object obj)
  { try { return (int)obj; }
    catch(InvalidCastException) { throw new ArgumentException("expected int but received "+TypeName(obj)); }
  }

  public static IProcedure ExpectProcedure(object obj)
  { IProcedure ret = MakeProcedure(obj);
    if(ret==null) throw new ArgumentException("expected function but received "+TypeName(obj));
    return ret;
  }

  public static Reference ExpectRef(object obj)
  { Reference ret = obj as Reference;
    if(ret==null) throw new ArgumentException("expected ref but received "+TypeName(obj));
    return ret;
  }

  public static string ExpectString(object obj)
  { string ret = obj as string;
    if(ret==null) throw new ArgumentException("expected string but received "+TypeName(obj));
    return ret;
  }

  public static ReflectedType ExpectType(object obj)
  { ReflectedType rt = obj as ReflectedType;
    if(rt!=null) return rt;
    Type type = obj as Type;
    if(type!=null) return Backend.ReflectedType.FromType(type);
    throw new ArgumentException("expected type but received "+TypeName(obj));
  }

  public static MultipleValues ExpectValues(object obj)
  { MultipleValues ret = obj as MultipleValues;
    if(ret==null) throw new ArgumentException("expected multiplevalues but received "+TypeName(obj));
    return ret;
  }

  public static object[] ExpectVector(object obj)
  { object[] ret = obj as object[];
    if(ret==null) throw new ArgumentException("expected vector but received "+TypeName(obj));
    return ret;
  }

  public static SyntaxErrorException SyntaxError(string message) { return new SyntaxErrorException(message); }
  public static SyntaxErrorException SyntaxError(string format, params object[] args)
  { return new SyntaxErrorException(string.Format(format, args));
  }
  public static SyntaxErrorException SyntaxError(Node node, string message)
  { return new SyntaxErrorException(message); // TODO: improve this with source information
  }
  public static SyntaxErrorException SyntaxError(Node node, string format, params object[] args)
  { return SyntaxError(node, string.Format(format, args));
  }
  #endregion

  #region Mathematical, etc ops
  public static object Add(object a, object b)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte: return IntOps.Add((int)(byte)a, b);
      case TypeCode.Char:
        if(b is string) return (char)a+(string)b;
        break;
      case TypeCode.Decimal:
        if(b is Decimal) return (Decimal)a + (Decimal)b;
        try { return (Decimal)a + Convert.ToDecimal(b); }
        catch(InvalidCastException) { break; }
      case TypeCode.Double: return FloatOps.Add((double)a, b);
      case TypeCode.Int16: return IntOps.Add((int)(short)a, b);
      case TypeCode.Int32: return IntOps.Add((int)a, b);
      case TypeCode.Int64: return LongOps.Add((long)a, b);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.Add((Integer)a, b);
        if(a is Complex) return ComplexOps.Add((Complex)a, b);
        break;
      case TypeCode.SByte: return IntOps.Add((int)(sbyte)a, b);
      case TypeCode.Single: return FloatOps.Add((float)a, b);
      case TypeCode.String:
        if(b is string) return (string)a + (string)b;
        if(b is char) return (string)a + (char)b;
        break;
      case TypeCode.UInt16: return IntOps.Add((int)(short)a, b);
      case TypeCode.UInt32: 
      { uint v = (uint)a;
        return v<=int.MaxValue ? IntOps.Add((int)v, b) : LongOps.Add((long)v, b);
      }
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? LongOps.Add((long)v, b) : IntegerOps.Add(new Integer(v), b);
      }
    }
    return GetCurrentLanguage().Add(a, b);
  }

  public static bool AreEqual(object a, object b)
  { if(a==b) return true;
    switch(Convert.GetTypeCode(a))
    { case TypeCode.Boolean: return b is bool && (bool)a==(bool)b;
      case TypeCode.Byte: return IntOps.AreEqual((byte)a, b);
      case TypeCode.Char: return IntOps.AreEqual((int)(char)a, b);
      case TypeCode.Decimal: return b is Decimal ? (Decimal)a==(Decimal)b
                                                 : FloatOps.AreEqual(Decimal.ToDouble((Decimal)a), b);
      case TypeCode.Double: return FloatOps.AreEqual((double)a, b);
      case TypeCode.Int16: return IntOps.AreEqual((short)a, b);
      case TypeCode.Int32: return IntOps.AreEqual((int)a, b);
      case TypeCode.Int64: return LongOps.AreEqual((long)a, b);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.AreEqual((Integer)a, b);
        if(a is Complex) return ComplexOps.AreEqual((Complex)a, b);
        return a.Equals(b);
      case TypeCode.SByte: return IntOps.AreEqual((sbyte)a, b);
      case TypeCode.Single: return FloatOps.AreEqual((float)a, b);
      case TypeCode.String: return b is string ? (string)a==(string)b : false;
      case TypeCode.UInt16: return IntOps.AreEqual((ushort)a, b);
      case TypeCode.UInt32:
      { uint v = (uint)a;
        return v<=int.MaxValue ? IntOps.AreEqual((int)v, b) : LongOps.AreEqual((long)v, b);
      }
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? LongOps.AreEqual((long)v, b) : IntegerOps.AreEqual(new Integer(v), b);
      }
    }
    return false;
  }

  public static object BitwiseAnd(object a, object b)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte:  return IntOps.BitwiseAnd((byte)a, b);
      case TypeCode.Int16: return IntOps.BitwiseAnd((short)a, b);
      case TypeCode.Int32: return IntOps.BitwiseAnd((int)a, b);
      case TypeCode.Int64: return LongOps.BitwiseAnd((long)a, b);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.BitwiseAnd((Integer)a, b);
        break;
      case TypeCode.SByte: return IntOps.BitwiseAnd((sbyte)a, b);
      case TypeCode.UInt16: return IntOps.BitwiseAnd((short)a, b);
      case TypeCode.UInt32:
      { uint v = (uint)a;
        return v<=int.MaxValue ? IntOps.BitwiseAnd((int)v, b) : LongOps.BitwiseAnd((long)v, b);
      }
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? LongOps.BitwiseAnd((long)v, b) : IntegerOps.BitwiseAnd(new Integer(v), b);
      }
    }
    return GetCurrentLanguage().BitwiseAnd(a, b);
  }

  public static object BitwiseOr(object a, object b)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte:  return IntOps.BitwiseOr((byte)a, b);
      case TypeCode.Int16: return IntOps.BitwiseOr((short)a, b);
      case TypeCode.Int32: return IntOps.BitwiseOr((int)a, b);
      case TypeCode.Int64: return LongOps.BitwiseOr((long)a, b);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.BitwiseOr((Integer)a, b);
        break;
      case TypeCode.SByte: return IntOps.BitwiseOr((sbyte)a, b);
      case TypeCode.UInt16: return IntOps.BitwiseOr((short)a, b);
      case TypeCode.UInt32:
      { uint v = (uint)a;
        return v<=int.MaxValue ? IntOps.BitwiseOr((int)v, b) : LongOps.BitwiseOr((long)v, b);
      }
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? LongOps.BitwiseOr((long)v, b) : IntegerOps.BitwiseOr(new Integer(v), b);
      }
    }
    return GetCurrentLanguage().BitwiseOr(a, b);
  }

  public static object BitwiseNegate(object a)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte:  return ~(int)(byte)a;
      case TypeCode.Int16: return ~(int)(short)a;
      case TypeCode.Int32: return ~(int)a;
      case TypeCode.Int64: return ~(long)a;
      case TypeCode.Object:
        if(a is Integer) return ~(Integer)a;
        break;
      case TypeCode.SByte: return ~(int)(sbyte)a;
      case TypeCode.UInt16: return ~(int)(short)a;
      case TypeCode.UInt32:
      { uint v = (uint)a;
        return v<=int.MaxValue ? (object)~(int)v : (object)~(long)v;
      }
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? (object)(~(long)v) : (object)(~new Integer(v));
      }
    }
    return GetCurrentLanguage().BitwiseNegate(a);
  }

  public static object BitwiseXor(object a, object b)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte:  return IntOps.BitwiseXor((byte)a, b);
      case TypeCode.Int16: return IntOps.BitwiseXor((short)a, b);
      case TypeCode.Int32: return IntOps.BitwiseXor((int)a, b);
      case TypeCode.Int64: return LongOps.BitwiseXor((long)a, b);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.BitwiseXor((Integer)a, b);
        break;
      case TypeCode.SByte: return IntOps.BitwiseXor((sbyte)a, b);
      case TypeCode.UInt16: return IntOps.BitwiseXor((short)a, b);
      case TypeCode.UInt32:
      { uint v = (uint)a;
        return v<=int.MaxValue ? IntOps.BitwiseXor((int)v, b) : LongOps.BitwiseXor((long)v, b);
      }
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? LongOps.BitwiseXor((long)v, b) : IntegerOps.BitwiseXor(new Integer(v), b);
      }
    }
    return GetCurrentLanguage().BitwiseXor(a, b);
  }

  public static int Compare(object a, object b)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Boolean:
        if(b is bool) return (bool)a ? (bool)b ? 0 : 1 : (bool)b ? -1 : 0;
        break;
      case TypeCode.Byte: return IntOps.Compare((int)(byte)a, b);
      case TypeCode.Char:
        if(b is char) return (int)(char)a - (int)(char)b;
        break;
      case TypeCode.Decimal:
        if(b is Decimal) return ((Decimal)a).CompareTo(b);
        try { return ((Decimal)a).CompareTo(Convert.ToDecimal(b)); }
        catch(InvalidCastException) { break; }
      case TypeCode.Double: return FloatOps.Compare((double)a, b);
      case TypeCode.Empty: return b==null ? 0 : -1;
      case TypeCode.Int16: return IntOps.Compare((short)a, b);
      case TypeCode.Int32: return IntOps.Compare((int)a, b);
      case TypeCode.Int64: return LongOps.Compare((long)a, b);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.Compare((Integer)a, b);
        if(a is Complex)
        { Complex c = (Complex)a;
          if(c.imag==0) return FloatOps.Compare(c.real, b);
        }
        break;
      case TypeCode.SByte: return IntOps.Compare((sbyte)a, b);
      case TypeCode.Single: return FloatOps.Compare((float)a, b);
      case TypeCode.String:
      { string sb = b as string;
        if(sb!=null) return string.Compare((string)a, sb);
        break;
      }
      case TypeCode.UInt16: return IntOps.Compare((short)a, b);
      case TypeCode.UInt32:
      { uint v = (uint)a;
        return v<=int.MaxValue ? IntOps.Compare((int)v, b) : LongOps.Compare((long)v, b);
      }
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? LongOps.Compare((long)v, b) : IntegerOps.Compare(new Integer(v), b);
      }
    }
    return GetCurrentLanguage().Compare(a, b);
  }

  public static object Divide(object a, object b)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte: return IntOps.Divide((byte)a, b, false);
      case TypeCode.Decimal:
        if(b is Decimal) return (Decimal)a / (Decimal)b;
        try { return (Decimal)a / Convert.ToDecimal(b); }
        catch(InvalidCastException) { break; }
      case TypeCode.Double:  return FloatOps.Divide((double)a, b, false);
      case TypeCode.Int16: return IntOps.Divide((short)a, b, false);
      case TypeCode.Int32: return IntOps.Divide((int)a, b, false);
      case TypeCode.Int64: return LongOps.Divide((long)a, b, false);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.Divide((Integer)a, b);
        if(a is Complex) return ComplexOps.Divide((Complex)a, b);
        break;
      case TypeCode.SByte: return IntOps.Divide((sbyte)a, b, false);
      case TypeCode.Single: return FloatOps.Divide((float)a, b, false);
      case TypeCode.UInt16: return IntOps.Divide((short)a, b, false);
      case TypeCode.UInt32: return LongOps.Divide((uint)a, b, false);
      case TypeCode.UInt64: return IntegerOps.Divide(new Integer((ulong)a), b);
    }
    return GetCurrentLanguage().Divide(a, b);
  }

  public static object Equal(object a, object b) { return FromBool(AreEqual(a, b)); }

  public static object FloorDivide(object a, object b)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte:   return IntOps.Divide((byte)a, b, true);
      case TypeCode.Double: return FloatOps.Divide((double)a, b, true);
      case TypeCode.Int16:  return IntOps.Divide((short)a, b, true);
      case TypeCode.Int32:  return IntOps.Divide((int)a, b, true);
      case TypeCode.Int64:  return LongOps.Divide((long)a, b, true);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.FloorDivide((Integer)a, b);
        if(a is Complex)
        { Complex c = (Complex)b;
          if(c.imag==0) return FloatOps.Divide(c.real, b, true);
        }
        else
        { IConvertible ic = b as IConvertible;
          if(ic!=null) return FloatOps.Divide(ic.ToDouble(NumberFormatInfo.InvariantInfo), b, true);
        }
        break;
      case TypeCode.SByte: return IntOps.Divide((sbyte)a, b, true);
      case TypeCode.Single: return FloatOps.Divide((float)a, b, true);
      case TypeCode.UInt16: return IntOps.Divide((short)a, b, true);
      case TypeCode.UInt32: 
      { uint v = (uint)a;
        return v<=int.MaxValue ? IntOps.Divide((int)v, b, true) : LongOps.Divide((long)v, b, true);
      }
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? LongOps.Divide((long)v, b, true) : IntegerOps.Divide(new Integer(v), b);
      }
    }
    return GetCurrentLanguage().FloorDivide(a, b);
  }

  public static bool IsTrue(object obj) { return GetCurrentLanguage().IsTrue(obj); }

  public static object LeftShift(object a, object b)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte:  return IntOps.LeftShift((int)(byte)a, b);
      case TypeCode.Int16: return IntOps.LeftShift((int)(short)a, b);
      case TypeCode.Int32: return IntOps.LeftShift((int)a, b);
      case TypeCode.Int64: return LongOps.LeftShift((long)a, b);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.LeftShift((Integer)a, b);
        break;
      case TypeCode.SByte: return IntOps.LeftShift((int)(sbyte)a, b);
      case TypeCode.UInt16: return IntOps.LeftShift((int)(short)a, b);
      case TypeCode.UInt32: return LongOps.LeftShift((long)(uint)a, b);
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? LongOps.LeftShift((long)v, b) : IntegerOps.LeftShift(new Integer(v), b);
      }
    }
    return GetCurrentLanguage().LeftShift(a, b);
  }

  public static object Less(object a, object b) { return FromBool(Compare(a,b)<0); }
  public static object LessEqual(object a, object b) { return FromBool(Compare(a,b)<=0); }

  public static object Modulus(object a, object b)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte:    return IntOps.Modulus((int)(byte)a, b);
      case TypeCode.Decimal: return FloatOps.Modulus(Decimal.ToDouble((Decimal)a), b);
      case TypeCode.Double:  return FloatOps.Modulus((double)a, b);
      case TypeCode.Int16:   return IntOps.Modulus((int)(short)a, b);
      case TypeCode.Int32:   return IntOps.Modulus((int)a, b);
      case TypeCode.Int64:   return LongOps.Modulus((long)a, b);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.Modulus((Integer)a, b);
        if(a is Complex)
        { Complex c = (Complex)a;
          if(c.imag==0) return FloatOps.Modulus(c.real, b);
        }
        break;
      case TypeCode.SByte: return IntOps.Modulus((int)(sbyte)a, b);
      case TypeCode.Single: return FloatOps.Modulus((float)a, b);
      case TypeCode.UInt16: return IntOps.Modulus((int)(short)a, b);
      case TypeCode.UInt32: 
      { uint v = (uint)a;
        return v<=int.MaxValue ? IntOps.Modulus((int)v, b) : LongOps.Modulus((long)v, b);
      }
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? LongOps.Modulus((long)v, b) : IntegerOps.Modulus(new Integer(v), b);
      }
    }
    return GetCurrentLanguage().Modulus(a, b);
  }

  public static object More(object a, object b) { return FromBool(Compare(a,b)>0); }
  public static object MoreEqual(object a, object b) { return FromBool(Compare(a,b)>=0); }

  public static object Multiply(object a, object b)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte:    return IntOps.Multiply((int)(byte)a, b);
      case TypeCode.Char:    return new string((char)a, ToInt(b));
      case TypeCode.Decimal:
        if(b is Decimal) return (Decimal)a * (Decimal)b;
        try { return (Decimal)a * Convert.ToDecimal(b); }
        catch(InvalidCastException) { break; }
      case TypeCode.Double: return FloatOps.Multiply((double)a, b);
      case TypeCode.Int16: return IntOps.Multiply((int)(short)a, b);
      case TypeCode.Int32: return IntOps.Multiply((int)a, b);
      case TypeCode.Int64: return LongOps.Multiply((long)a, b);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.Multiply((Integer)a, b);
        if(a is Complex) return ComplexOps.Multiply((Complex)a, b);
        break;
      case TypeCode.SByte: return IntOps.Multiply((int)(sbyte)a, b);
      case TypeCode.Single: return FloatOps.Multiply((float)a, b);
      case TypeCode.String: return StringOps.Multiply((string)a, b);
      case TypeCode.UInt16: return IntOps.Multiply((int)(short)a, b);
      case TypeCode.UInt32: 
      { uint v = (uint)a;
        return v<=int.MaxValue ? IntOps.Multiply((int)v, b) : LongOps.Multiply((long)v, b);
      }
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? LongOps.Multiply((long)v, b) : IntegerOps.Multiply(new Integer(v), b);
      }
    }
    return GetCurrentLanguage().Multiply(a, b);
  }

  public static object Negate(object a)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte: return -(int)(byte)a;
      case TypeCode.Decimal: return -(Decimal)a;
      case TypeCode.Double: return -(double)a;
      case TypeCode.Int16: return -(int)(short)a;
      case TypeCode.Int32: return -(int)a;
      case TypeCode.Int64: return -(long)a;
      case TypeCode.Object:
        if(a is Integer) return -(Integer)a;
        if(a is Complex) return -(Complex)a;
        break;
      case TypeCode.SByte: return -(int)(sbyte)a;
      case TypeCode.Single: return -(float)a;
      case TypeCode.UInt16: return -(int)(short)a;
      case TypeCode.UInt32:
      { uint v = (uint)a;
        return v<=int.MaxValue ? (object)-(int)v : (object)-(long)v;
      }
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? (object)-(long)v : (object)-new Integer(v);
      }
    }
    return GetCurrentLanguage().Negate(a);
  }

  public static object NotEqual(object a, object b) { return FromBool(!AreEqual(a, b)); }

  public static object Power(object a, object b)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte:    return IntOps.Power((int)(byte)a, b);
      case TypeCode.Double:  return FloatOps.Power((double)a, b);
      case TypeCode.Int16: return IntOps.Power((int)(short)a, b);
      case TypeCode.Int32: return IntOps.Power((int)a, b);
      case TypeCode.Int64: return LongOps.Power((long)a, b);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.Power((Integer)a, b);
        if(a is Complex) return ComplexOps.Power((Complex)a, b);
        break;
      case TypeCode.SByte: return IntOps.Power((int)(sbyte)a, b);
      case TypeCode.Single: return FloatOps.Power((float)a, b);
      case TypeCode.UInt16: return IntOps.Power((int)(short)a, b);
      case TypeCode.UInt32: 
      { uint v = (uint)a;
        return v<=int.MaxValue ? IntOps.Power((int)v, b) : LongOps.Power((long)v, b);
      }
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? LongOps.Power((long)v, b) : IntegerOps.Power(new Integer(v), b);
      }
    }
    return GetCurrentLanguage().Power(a, b);
  }

  // TODO: optimize this
  public static object PowerMod(object a, object b, object c)
  { if(a is Integer) return IntegerOps.PowerMod((Integer)a, b, c);
    if(a is Complex) return ComplexOps.PowerMod((Complex)a, b, c);
    return Modulus(Power(a, b), c);
  }

  public static object RightShift(object a, object b)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte:  return IntOps.RightShift((int)(byte)a, b);
      case TypeCode.Int16: return IntOps.RightShift((int)(short)a, b);
      case TypeCode.Int32: return IntOps.RightShift((int)a, b);
      case TypeCode.Int64: return LongOps.RightShift((long)a, b);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.RightShift((Integer)a, b);
        break;
      case TypeCode.SByte: return IntOps.RightShift((int)(sbyte)a, b);
      case TypeCode.UInt16: return IntOps.RightShift((int)(short)a, b);
      case TypeCode.UInt32: return LongOps.RightShift((long)(uint)a, b);
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? LongOps.RightShift((long)v, b) : IntegerOps.RightShift(new Integer(v), b);
      }
    }
    return GetCurrentLanguage().RightShift(a, b);
  }

  public static object Subtract(object a, object b)
  { switch(Convert.GetTypeCode(a))
    { case TypeCode.Byte:    return IntOps.Subtract((int)(byte)a, b);
      case TypeCode.Decimal:
        if(b is Decimal) return (Decimal)a - (Decimal)b;
        try { return (Decimal)a - Convert.ToDecimal(b); }
        catch(InvalidCastException) { break; }
      case TypeCode.Double:  return FloatOps.Subtract((double)a, b);
      case TypeCode.Int16: return IntOps.Subtract((int)(short)a, b);
      case TypeCode.Int32: return IntOps.Subtract((int)a, b);
      case TypeCode.Int64: return LongOps.Subtract((long)a, b);
      case TypeCode.Object:
        if(a is Integer) return IntegerOps.Subtract((Integer)a, b);
        if(a is Complex) return ComplexOps.Subtract((Complex)a, b);
        break;
      case TypeCode.SByte: return IntOps.Subtract((int)(sbyte)a, b);
      case TypeCode.Single: return FloatOps.Subtract((float)a, b);
      case TypeCode.UInt16: return IntOps.Subtract((int)(short)a, b);
      case TypeCode.UInt32: 
      { uint v = (uint)a;
        return v<=int.MaxValue ? IntOps.Subtract((int)v, b) : LongOps.Subtract((long)v, b);
      }
      case TypeCode.UInt64:
      { ulong v = (ulong)a;
        return v<=long.MaxValue ? LongOps.Subtract((long)v, b) : IntegerOps.Subtract(new Integer(v), b);
      }
    }
    return GetCurrentLanguage().Subtract(a, b);
  }
  #endregion

  #region Miscellaneous
  public static object FromBool(bool value) { return value ? TRUE : FALSE; }

  // FIXME: when inside the compiler, it should always return Options.Current.Language (i think?)
  // eg, if the compiler is invoked by some script, and the language being compiled is different from the language
  // invoking the compiler...
  public static Language GetCurrentLanguage()
  { return CurrentFunction==null ? Options.Current.Language : CurrentFunction.Language;
  }

  public static object GetGlobal(string name) { return TopLevel.Current.Get(name); }
  public static bool GetGlobal(string name, out object value) { return TopLevel.Current.Get(name, out value); }

  public static object GetIndex(object col, object index)
  { IList list = col as IList;
    if(list!=null) return list[ToInt(index)];

    IDictionary dict = col as IDictionary;
    if(dict!=null) return dict[index];

    if(CallProperty(col, "[]", out index, new object[] { col, index })) return index;

    throw new ArgumentException("objects of type '"+TypeName(col)+"' are not indexable");
  }

  public static void SetIndex(object value, object col, object index)
  { IList list = col as IList;
    if(list!=null) list[ToInt(index)] = value;
    else
    { IDictionary dict = col as IDictionary;
      if(dict!=null) dict[index] = value;
      else if(!CallProperty(col, "[]", out value, new object[] { col, index, value }))
        throw new ArgumentException("objects of type '"+TypeName(col)+"' are not indexable");
    }
  }

  public static string Str(object o) { return GetCurrentLanguage().Str(o); }
  public static string ToCode(object o) { return GetCurrentLanguage().ToCode(o); }

  public static string ToHex(uint number, int minlen)
  { const string cvt = "0123456789ABCDEF";

    unsafe
    { char* chars = stackalloc char[8];
      int len = 0;
      do
      { chars[8 - ++len] = cvt[(int)(number&0xF)];
        number >>= 4;
      } while(number!=0 && len<minlen);
      return new string(chars, 8-len, len);
    }
  }

  public static string ToHex(byte[] data)
  { if(data==null) throw new ArgumentNullException("data");

    const string cvt = "0123456789ABCDEF";

    char[] chars = new char[data.Length*2];
    unsafe
    { fixed(byte* src=data)
      fixed(char* dest=chars, map=cvt)
      { for(int i=0,j=0,len=data.Length; i<len; i++)
        { byte b = src[i];
          dest[j++] = map[b>>4];
          dest[j++] = map[b&0xF];
        }
      }
      return new string(chars);
    }
  }

  public static string TypeName(object o) { return TypeName(o==null ? null : o.GetType()); }
  public static string TypeName(ReflectedType type) { return TypeName(type.Type); }
  public static string TypeName(Type type) { return GetCurrentLanguage().TypeName(type); }
  #endregion
  
  #region Procedures
  public static object Call(string name) { return Call(GetGlobal(name), EmptyArray); }
  public static object Call(string name, params object[] args) { return Call(GetGlobal(name), args); }
  public static object Call(object func) { return ExpectProcedure(func).Call(EmptyArray); }
  public static object Call(object func, params object[] args) { return ExpectProcedure(func).Call(args); }

  public static object Call(object func, CallArg[] args)
  { object[] positional, values;
    string[] names;
    EvaluateCallArgs(args, out positional, out names, out values);
    return names==null ? ExpectProcedure(func).Call(positional)
                       : ExpectFancyProcedure(func).Call(positional, names, values);
  }

  public static void EvaluateCallArgs(CallArg[] args, out object[] positional,
                                      out string[] names, out object[] values)
  { int ai=0, pi=0, num=0;
    bool hasdict = false;

    for(; ai<args.Length; ai++)
      if(args[ai].Type==null) num++;
      else if(args[ai].Type==CallArg.ListType)
      { ICollection col = args[ai].Value as ICollection;
        if(col!=null) num += col.Count;
        else throw new ArgumentException("Expected ICollection list type, but received "+TypeName(args[ai].Value));
      }
      else if(args[ai].Type is int) num += (int)args[ai].Type;
      else break;

    positional = num==0 ? null : new object[num];
    for(int i=0; i<ai; i++)
      if(args[i].Type==null) positional[pi++] = args[i].Value;
      else if(args[i].Type==CallArg.ListType)
      { ICollection col = (ICollection)args[i].Value as ICollection;
        col.CopyTo(positional, pi);
        pi += col.Count;
      }
      else if(args[i].Type is int)
      { object[] items = (object[])args[i].Value;
        items.CopyTo(positional, pi);
        pi += items.Length;
      }

    if(ai==args.Length) { names=null; values=null; return; }

    num = 0;
    for(int i=ai; i<args.Length; i++)
      if(args[i].Type==CallArg.DictType)
      { IDictionary dict = args[i].Value as IDictionary;
        if(dict!=null) num += dict.Count;
        else throw new ArgumentException("Expected IDictionary dict type, but received "+TypeName(args[i].Value));
        hasdict = true;
      }
      else num += ((object[])args[i].Type).Length;

    if(!hasdict) { names=(string[])args[ai].Value; values=(object[])args[ai].Type; return; }

    names = new string[num];
    values = new object[num];
    pi = 0;
    for(; ai<args.Length; ai++)
      if(args[ai].Type!=CallArg.DictType)
      { string[] na = (string[])args[ai].Value;
        na.CopyTo(names, pi);
        ((object[])args[ai].Type).CopyTo(values, pi);
        pi += na.Length;
      }
      else
      { IDictionary dict = (IDictionary)args[ai].Value;
        foreach(DictionaryEntry e in dict)
        { names[pi] = Ops.Str(e.Key);
          values[pi] = e.Value;
          pi++;
        }
      }
  }

  public static bool IsProcedure(object o) { return o is IProcedure || o is Delegate; }
  
  public static IProcedure MakeProcedure(object o)
  { if(o!=null)
    { IProcedure proc = o as IProcedure;
      if(proc!=null) return proc;
      
      Delegate del = o as Delegate;
      if(del!=null) return Interop.MakeProcedure(del);
    }    
    return null;
  }

  public static IProcedure MakeProcedure(Delegate del) { return Interop.MakeProcedure(del); }
  #endregion

  #region Slots, properties, etc
  public static object CallProperty(object obj, string name) { return CallProperty(obj, name, EmptyArray); }
  public static bool CallProperty(object obj, string name, out object ret)
  { return CallProperty(obj, name, out ret, EmptyArray);
  }

  public static object CallProperty(object obj, string name, params object[] args)
  { return MemberContainer.FromObject(obj).CallProperty(obj, name, args);
  }
  public static bool CallProperty(object obj, string name, out object ret, params object[] args)
  { return MemberContainer.FromObject(obj).CallProperty(obj, name, out ret, args);
  }

  public static object CallProperty(object obj, string name, object[] positional, string[] names, object[] values)
  { return MemberContainer.FromObject(obj).CallProperty(obj, name, positional, names, values);
  }
  public static bool CallProperty(object obj, string name, out object ret,
                                    object[] positional, string[] names, object[] values)
  { return MemberContainer.FromObject(obj).CallProperty(obj, name, out ret, positional, names, values);
  }

  public static object CallProperty(object obj, string name, CallArg[] args)
  { MemberContainer mc = MemberContainer.FromObject(obj);
    object[] pos, values;
    string[] names;
    EvaluateCallArgs(args, out pos, out names, out values);
    return names==null ? mc.CallProperty(obj, name, pos) : mc.CallProperty(obj, name, pos, names, values);
  }

  public static bool CallProperty(object obj, string name, out object ret, CallArg[] args)
  { MemberContainer mc = MemberContainer.FromObject(obj);
    object[] positional, values;
    string[] names;
    EvaluateCallArgs(args, out positional, out names, out values);
    return names==null ? mc.CallProperty(obj, name, out ret, positional)
                       : mc.CallProperty(obj, name, out ret, positional, names, values);
  }

  public static void DeleteSlot(object obj, string name) { MemberContainer.FromObject(obj).DeleteSlot(obj, name); }

  public static IProcedure GetAccessor(object obj, string name)
  { return MemberContainer.FromObject(obj).GetAccessor(obj, name);
  }
  public static bool GetAccessor(object obj, string name, out IProcedure proc)
  { return MemberContainer.FromObject(obj).GetAccessor(obj, name, out proc);
  }

  public static ICollection<string> GetMemberNames(object obj) { return GetMemberNames(obj, false); }
  public static ICollection<string> GetMemberNames(object obj, bool includeImports)
  { return MemberContainer.FromObject(obj).GetMemberNames(includeImports);
  }

  public static object GetProperty(object obj, string name)
  { return MemberContainer.FromObject(obj).GetProperty(obj, name);
  }
  public static bool GetProperty(object obj, string name, out object value)
  { return MemberContainer.FromObject(obj).GetProperty(obj, name, out value);
  }

  public static void SetProperty(object value, object obj, string name)
  { MemberContainer.FromObject(obj).SetProperty(obj, name, value);
  }
  public static void TrySetProperty(object value, object obj, string name)
  { MemberContainer.FromObject(obj).TrySetProperty(obj, name, value);
  }

  public static object GetSlot(object obj, string name) { return MemberContainer.FromObject(obj).GetSlot(obj, name); }
  public static bool GetSlot(object obj, string name, out object value)
  { return MemberContainer.FromObject(obj).GetSlot(obj, name, out value);
  }

  public static void SetSlot(object value, object obj, string name)
  { MemberContainer.FromObject(obj).SetSlot(obj, name, value);
  }
  public static bool TrySetSlot(object value, object obj, string name)
  { return MemberContainer.FromObject(obj).TrySetSlot(obj, name, value);
  }

  public static object Invoke(object target, string name) { return Invoke(target, name, EmptyArray); }
  public static object Invoke(object target, string name, params object[] args)
  { return Call(GetSlot(target, name), args);
  }

  public static bool Invoke(object target, string name, out object retValue)
  { return Invoke(target, name, out retValue, EmptyArray);
  }
  public static bool Invoke(object target, string name, out object retValue, params object[] args)
  { object method;
    if(GetSlot(target, name, out method)) { retValue = Call(method, args); return true; }
    else { retValue = null; return false; }
  }

  public static object InvokeProperty(object target, string name) { return InvokeProperty(target, name, EmptyArray); }
  public static object InvokeProperty(object target, string name, params object[] args)
  { return Call(GetProperty(target, name), args);
  }

  public static bool InvokeProperty(object target, string name, out object retValue)
  { return InvokeProperty(target, name, out retValue, EmptyArray);
  }
  public static bool InvokeProperty(object target, string name, out object retValue, params object[] args)
  { object method;
    if(GetProperty(target, name, out method)) { retValue = Call(method, args); return true; }
    else { retValue = null; return false; }
  }
  #endregion

  #region Type conversion, creation, etc
  public static object ConvertTo(object o, Type type)
  { switch(ConvertTo(o==null ? null : o.GetType(), type))
    { case Conversion.Identity: case Conversion.Reference: return o;
      case Conversion.None:
        throw new InvalidCastException(string.Format("object cannot be converted to '{0}'", TypeName(type)));
      default:
        if(type==typeof(bool)) return FromBool(IsTrue(o));
        else if(type.IsSubclassOf(typeof(Delegate))) return MakeDelegate(o, type);
        else if(type==typeof(IProcedure))
        { Delegate del = o as Delegate;
          if(del!=null) return Interop.MakeProcedure(del);
        }
        return Convert.ChangeType(o, type);
    }
  }

  public static Conversion ConvertTo(Type from, Type to)
  { if(from==null)
      return !to.IsValueType ? Conversion.Reference : to==typeof(bool) ? Conversion.Safe : Conversion.None;
    else if(to==from) return Conversion.Identity;
    else if(to.IsAssignableFrom(from)) return Conversion.Reference;

    // TODO: check whether it's faster to use IndexOf() or our own loop
    // TODO: add support for Integer, Complex, and Decimal
    if(to.IsPrimitive)
    { if(from.IsPrimitive)
      { if(to==typeof(bool)) return IsIn(typeConv[9], from) ? Conversion.None : Conversion.Safe;
        else
          switch(Type.GetTypeCode(from))
          { case TypeCode.Int32:  return IsIn(typeConv[4], to) ? Conversion.Safe : Conversion.Unsafe;
            case TypeCode.Double: return Conversion.Unsafe;
            case TypeCode.Int64:  return IsIn(typeConv[6], to) ? Conversion.Safe : Conversion.Unsafe;
            case TypeCode.Char:   return IsIn(typeConv[8], to) ? Conversion.Safe : Conversion.Unsafe;
            case TypeCode.Byte:   return IsIn(typeConv[1], to) ? Conversion.Safe : Conversion.Unsafe;
            case TypeCode.UInt32: return IsIn(typeConv[5], to) ? Conversion.Safe : Conversion.Unsafe;
            case TypeCode.Single: return to==typeof(double) ? Conversion.Safe : Conversion.Unsafe;
            case TypeCode.Int16:  return IsIn(typeConv[2], to) ? Conversion.Safe : Conversion.Unsafe;
            case TypeCode.UInt16: return IsIn(typeConv[3], to) ? Conversion.Safe : Conversion.Unsafe;
            case TypeCode.SByte:  return IsIn(typeConv[0], to) ? Conversion.Safe : Conversion.Unsafe;
            case TypeCode.UInt64: return IsIn(typeConv[7], to) ? Conversion.Safe : Conversion.Unsafe;
          }
       }
       else if(from==typeof(object)) return Conversion.Unsafe;
    }
    if(from.IsArray && to.IsArray && to.GetElementType().IsAssignableFrom(from.GetElementType()))
      return Conversion.Reference;
    else if(to.IsSubclassOf(typeof(Delegate)) && typeof(IProcedure).IsAssignableFrom(from) ||
            to==typeof(IProcedure) && from.IsSubclassOf(typeof(Delegate)))
      return Conversion.Unsafe;
    return Conversion.None;
  }

  public static Delegate MakeDelegate(object callable, Type delegateType)
  { IProcedure proc = callable as IProcedure;
    if(proc!=null) return Interop.MakeDelegate(proc, delegateType);
    
    Delegate del = callable as Delegate;
    if(del!=null && delegateType.IsAssignableFrom(del.GetType())) return del;

    throw new ArgumentException("expected a procedure or a delegate compatible with "+TypeName(delegateType));
  }

  public static Exception MakeException(object exobj, object[] args)
  { Exception ex = exobj as Exception;

    string msg;
    if(args==null || args.Length==0)
    { if(ex!=null) return ex;
      msg = null;
    }
    else
    { System.Text.StringBuilder sb = new System.Text.StringBuilder();
      foreach(object o in args) sb.Append(Str(o));
      msg = sb.ToString();

      if(ex!=null) // TODO: see if we can find a better way to handle this case
        throw new ArgumentException("Can't specify additional data for an exception that's already been created. An "+
                                    "attempt was made to throw an exception of type "+TypeName(ex)+" with the "+
                                    "following extra data:\n\n"+msg+"\n\nYou can either throw an exception type, "+
                                    "with extra data, or an exception object without extra data.");
    }

    ReflectedType type = exobj as ReflectedType;
    if(type==null)
    { if(exobj is Type) type = ReflectedType.FromType((Type)exobj);
      if(type==null)
        throw new ArgumentException("An attempt was made to throw an exception from an object of type "+
                                    TypeName(exobj)+", which is neither a System.Exception nor a type which, when "+
                                    "instantiated, will produce a System.Exception. "+
                                    (msg==null ? "There was no extra data." : "The extra data was:\n"+msg));
    }

    IProcedure cons = type.Constructor;
    if(cons==null) throw new ArgumentException("Cannot create an exception from "+Ops.TypeName(type)+
                                               " because it has no constructor");
    // TODO: make less assumptions about the types of constructors it has
    if(args==null || args.Length==0) ex = cons.Call(EmptyArray) as Exception;
    else
    { System.Text.StringBuilder sb = new System.Text.StringBuilder();
      foreach(object o in args) sb.Append(Str(o));
      ex = cons.Call(sb.ToString()) as Exception;
    }
    if(ex==null) throw new ArgumentException("Cannot create an exception from "+Ops.TypeName(type)+
                                             " because it is not derived from System.Exception");
    return ex;
  }

  public static double ToFloat(object o)
  { if(o is double) return (double)o;
    if(o is Complex)
    { Complex c = (Complex)o;
      if(c.imag==0) return c.real;
    }
    return Convert.ToDouble(o);
  }

  public static int ToInt(object o)
  { if(o is int) return (int)o;

    switch(Convert.GetTypeCode(o))
    { case TypeCode.Boolean: return (bool)o ? 1 : 0;
      case TypeCode.Byte: return (byte)o;
      case TypeCode.Char: return (char)o;
      case TypeCode.Decimal: return (int)(Decimal)o;
      case TypeCode.Double: return checked((int)(double)o);
      case TypeCode.Int16: return (short)o;
      case TypeCode.Int64: return checked((int)(long)o);
      case TypeCode.SByte: return (sbyte)o;
      case TypeCode.Single: return checked((int)(float)o);
      case TypeCode.String: return int.Parse((string)o);
      case TypeCode.UInt16: return (int)(ushort)o;
      case TypeCode.UInt32: return checked((int)(uint)o);
      case TypeCode.UInt64: return checked((int)(ulong)o);
      default: return checked((int)Convert.ToSingle(o)); // we do it this way so it truncates
    }
  }

  public static long ToLong(object o)
  { switch(Convert.GetTypeCode(o))
    { case TypeCode.Boolean: return (bool)o ? 1 : 0;
      case TypeCode.Byte: return (byte)o;
      case TypeCode.Char: return (char)o;
      case TypeCode.Decimal: return (long)(Decimal)o;
      case TypeCode.Double: return checked((long)(double)o);
      case TypeCode.Int16: return (short)o;
      case TypeCode.Int32: return (int)o;
      case TypeCode.Int64: return (long)o;
      case TypeCode.SByte: return (sbyte)o;
      case TypeCode.Single: return checked((long)(float)o);
      case TypeCode.String: return long.Parse((string)o);
      case TypeCode.UInt16: return (long)(ushort)o;
      case TypeCode.UInt32: return (long)(uint)o;
      case TypeCode.UInt64: return checked((long)(ulong)o);
      default: return checked((long)Convert.ToSingle(o));
    }
  }
  #endregion

  #region Fields, privates
  public static readonly object Missing = new Singleton("<Missing>");
  public static readonly object FALSE=false, TRUE=true;
  public static readonly object[] EmptyArray = new object[0];

  [ThreadStatic] public static Template CurrentFunction;

  static bool IsIn(Type[] typeArr, Type type)
  { for(int i=0; i<typeArr.Length; i++) if(typeArr[i]==type) return true;
    return false;
  }

  static readonly Type[][] typeConv = 
  { // FROM
    new Type[] { typeof(int), typeof(double), typeof(short), typeof(long), typeof(float) }, // sbyte
    new Type[] // byte
    { typeof(int), typeof(double), typeof(uint), typeof(short), typeof(ushort), typeof(long), typeof(ulong),
      typeof(float)
    },
    new Type[] { typeof(int), typeof(double), typeof(long), typeof(float) }, // short
    new Type[] { typeof(int), typeof(double), typeof(uint), typeof(long), typeof(ulong), typeof(float) }, // ushort
    new Type[] { typeof(double), typeof(long), typeof(float) }, // int
    new Type[] { typeof(double), typeof(long), typeof(ulong), typeof(float) }, // uint
    new Type[] { typeof(double), typeof(float) }, // long
    new Type[] { typeof(double), typeof(float) }, // ulong
    new Type[] // char
    { typeof(int), typeof(double), typeof(ushort), typeof(uint), typeof(long), typeof(ulong), typeof(float)
    },

    // TO
    new Type[] // bool
    { typeof(int), typeof(byte), typeof(char), typeof(sbyte), typeof(short), typeof(ushort), typeof(uint),
      typeof(long), typeof(ulong)
    }
  };
  #endregion
}
#endregion

#region Primitive
public abstract class Primitive : SimpleProcedure
{ public Primitive(string name, int min, int max) : base(name, min, max) { }
  public override string ToString() { return string.Format("#<primitive procedure '{0}'>", name); }
}
#endregion

#region ScriptComparer
public sealed class ScriptComparer : IComparer
{ public ScriptComparer(IProcedure proc)
  { if(proc!=null) { this.proc=proc; args=new object[2]; }
  }

  public int Compare(object a, object b)
  { if(proc==null) return Ops.Compare(a, b);
    else
    { args[0] = a;
      args[1] = b;
      return Ops.ToInt(proc.Call(args));
    }
  }

  public static readonly ScriptComparer Default = new ScriptComparer(null);

  IProcedure proc;
  object[] args;
}
#endregion

} // namespace Scripting

namespace Scripting.Backend
{

#region Enums
[Flags]
public enum Conversion
{ None=0,
  // 1/3 and 8/12 are chosen to make it clear that those are mutually exclusive
  Unsafe=1, Safe=2, Reference=3, Identity=4, UnsafeAPA=8, SafeAPA=16, RefAPA=24, PacksPA=32,
  QualityMask=31
}
#endregion

#region Cast
public sealed class Cast : MemberContainer
{ public Cast(ReflectedType type, object value)
  { ReflectedType=type; Type=type.Type; Value=value;
    if(value!=null && !Type.IsAssignableFrom(value.GetType()))
      throw new InvalidCastException("Attempted to cast "+Ops.TypeName(value)+" to "+Ops.TypeName(Type));
  }

  public override bool GetSlot(object instance, string name, out object ret)
  { return ReflectedType.GetSlot(instance, name, out ret);
  }

  public override ICollection<string> GetMemberNames(bool includeImports)
  { return ReflectedType.GetMemberNames(includeImports);
  }

  public override void Export(TopLevel top, string[] names, string[] asNames)
  { ReflectedType.Export(top, names, asNames);
  }

  public override bool TryDeleteSlot(object instance, string name)
  { return ReflectedType.TryDeleteSlot(instance, name);
  }

  public override bool TrySetSlot(object instance, string name, object value)
  { return ReflectedType.TrySetSlot(instance, name, value);
  }

  public override string ToString() { return "#<cast '"+Ops.Str(Value)+"' to "+Ops.TypeName(Type)+">"; }

  public ReflectedType ReflectedType;
  public Type Type;
  public object Value;

  public readonly static Type ClassType = typeof(Cast);
}
#endregion

#region CallArg
public struct CallArg
{ public CallArg(object value, object type) { Value=value; Type=type; }
  public object Value, Type;
  
  public static readonly object DictType=new Singleton("CallArg.DictType"), ListType=new Singleton("CallArg.ListType");
}
#endregion

#region CodeModule
public class CodeModule : MemberContainer
{ public CodeModule(string name) { Name=name; TopLevel=new TopLevel(); }
  public CodeModule(string name, TopLevel top) { Name=name; TopLevel=top; }

  public override bool GetSlot(object instance, string name, out object ret)
  { return TopLevel.Globals.Get(name, out ret);
  }

  public override ICollection<string> GetMemberNames(bool includeImports)
  { if(includeImports) return TopLevel.Globals.Dict.Keys;

    List<string> ret = new List<string>();
    foreach(KeyValuePair<string,Binding> de in TopLevel.Globals.Dict)
      if(de.Value.From==TopLevel) ret.Add(de.Key);
    return ret;
  }

  public override void Export(TopLevel top, string[] names, string[] asNames) { TopLevel.Export(top, names, asNames); }

  public override bool TryDeleteSlot(object instance, string name) { TopLevel.Globals.Unbind(name); return true; }

  public override bool TrySetSlot(object instance, string name, object value)
  { TopLevel.Set(name, value);
    return true;
  }

  public override string ToString() { return "#<module '"+Name+"'>"; }

  public readonly TopLevel TopLevel;
  public readonly string Name;
}
#endregion

#region DynamicMethodGenerator
public sealed class DynamicMethodGenerator : Generator
{ public DynamicMethodGenerator(DynamicMethod dm, Binding[] bindings, object[] constants, int dataSize)
  { method=dm; Bindings=bindings; Constants=constants; nsData = dataSize==0 ? null : new object[dataSize];
  }

  public delegate bool MoveNextDelegate(LocalEnvironment env);

  public DynamicMethodGenerator Clone(LocalEnvironment env)
  { DynamicMethodGenerator dmg = (DynamicMethodGenerator)MemberwiseClone();
    dmg.environment = env;
    dmg.nextProc  = (MoveNextDelegate)method.CreateDelegate(typeof(MoveNextDelegate), dmg);
    return dmg;
  }

  protected override bool InnerNext(LocalEnvironment env) { return nextProc(env); }

  readonly Binding[] Bindings;
  readonly object[] Constants, nsData;
  readonly DynamicMethod method;
  MoveNextDelegate nextProc;
}
#endregion

#region Generator
public abstract class Generator : IEnumerator
{ protected Generator() { jump = uint.MaxValue; }
  protected Generator(LocalEnvironment env) { environment=env; jump=uint.MaxValue; }

  public object Current
  { get
    { if(state!=State.In) throw new InvalidOperationException();
      return current;
    }
  }
  
  public bool MoveNext()
  { try
    { if(state==State.Done || !InnerNext(environment)) { state=State.Done; return false; }
      state = State.In;
      return true;
    }
    catch { state=State.Done; throw; }
  }
  
  public void Reset() { throw new NotSupportedException(); }

  protected abstract bool InnerNext(LocalEnvironment ENV);
  protected uint jump;
  protected object current;
  protected LocalEnvironment environment;
  
  enum State : byte { Before, In, Done };
  State state;
}
#endregion

#region InstanceWrapper
public sealed class InstanceWrapper : IProcedure
{ public InstanceWrapper(object instance, IProcedure proc) { Instance=instance; Procedure=proc; }

  public int MinArgs { get { return Math.Max(0, Procedure.MinArgs-1); } }

  public int MaxArgs
  { get
    { int max = Procedure.MaxArgs;
      return max==-1 ? -1 : Math.Max(0, max-1);
    }
  }

  public bool NeedsFreshArgs { get { return false; } }
  
  public object Call(object[] args)
  { object[] nargs = new object[args.Length+1];
    nargs[0] = Instance;
    if(args.Length!=0) args.CopyTo(nargs, 1);
    return Procedure.Call(nargs);
  }

  public override string ToString() { return "#<method wrapper for "+Procedure.ToString()+">"; }

  public readonly object Instance;
  public readonly IProcedure Procedure;
}
#endregion

#region Procedures
public delegate object ProcedureDelegate(LocalEnvironment env, params object[] args);

#region Lambda
public abstract class Lambda : IFancyProcedure
{ public int MinArgs { get { return Template.NumParams; } }
  public int MaxArgs { get { return Template.HasList ? -1 : Template.NumParams; } }
  public bool NeedsFreshArgs { get { return Template.ArgsClosed; } }

  public abstract object Call(params object[] args);
  public abstract object Call(object[] positional, string[] keywords, object[] values);

  public override string ToString() { return Template.Name==null ? "#<function>" : "#<function '"+Template.Name+"'>"; }

  public Template Template;
}
#endregion

#region ClosureBase
public abstract class ClosureBase : Lambda
{ public LocalEnvironment Environment;
  public object[] Defaults;
}
#endregion

#region DynamicMethodClosure
public sealed class DynamicMethodClosure : ClosureBase
{ public DynamicMethodClosure(DynamicMethod dm, Template template, Binding[] bindings, object[] constants)
  { Proc = (ProcedureDelegate)dm.CreateDelegate(typeof(ProcedureDelegate), this);
    Template  = template;
    Bindings  = bindings;
    Constants = constants;
  }

  // the Proc delegate of the new closure still references this object as its instance, but since the function only
  // needs to access Bindings and Constants, which are readonly, it's okay.
  public DynamicMethodClosure Clone(LocalEnvironment env, object[] defaults)
  { DynamicMethodClosure dm = (DynamicMethodClosure)MemberwiseClone();
    dm.Environment = env;
    dm.Defaults    = defaults;
    return dm;
  }

  public override object Call(params object[] args)
  { Template oldTemplate = Ops.CurrentFunction;
    TopLevel oldTop = TopLevel.Current;
    try
    { Ops.CurrentFunction = Template;
      TopLevel.Current = Template.TopLevel;
      return Proc(Environment, Template.FixArgs(args, Defaults));
    }
    finally
    { Ops.CurrentFunction = oldTemplate;
      TopLevel.Current = oldTop;
    }
  }

  public override object Call(object[] positional, string[] keywords, object[] values)
  { Template oldTemplate = Ops.CurrentFunction;
    TopLevel oldTop = TopLevel.Current;
    try
    { Ops.CurrentFunction = Template;
      TopLevel.Current = Template.TopLevel;
      return Proc(Environment, Template.MakeArgs(positional, Defaults, keywords, values));
    }
    finally
    { Ops.CurrentFunction = oldTemplate;
      TopLevel.Current = oldTop;
    }
  }

  readonly ProcedureDelegate Proc;
  readonly object[] Bindings;
  readonly object[] Constants;
}
#endregion

#region InterpretedProcedure
public sealed class InterpretedProcedure : Lambda
{ public InterpretedProcedure(string[] paramNames, Node body)
    : this(Options.Current.Language, null, paramNames, false, false, body) { }
  public InterpretedProcedure(Language lang, string name, string[] paramNames, bool hasList, bool hasDict, Node body)
  { int numRequired = paramNames.Length-(hasList ? 1 : 0)-(hasDict ? 1 : 0);
    if(numRequired<0) throw new ArgumentException("Not enough parameters to accomodate list and dictionary");
    Template = new Template(IntPtr.Zero, lang, name, paramNames, numRequired, hasList, hasDict, false);
    Body = body;
  }
  public InterpretedProcedure(string name, Parameter[] parms, Node body)
    : this(Options.Current.Language, name, parms, body) { }
  public InterpretedProcedure(Language lang, string name, Parameter[] parms, Node body)
  { int numRequired, optionalStart, numOptional;
    bool hasList, hasDict;
    Parameter.CheckParms(parms, out numRequired, out optionalStart, out numOptional, out hasList, out hasDict);

    Template = new Template(IntPtr.Zero, lang, name, Parameter.GetNames(parms), numRequired, hasList, hasDict, false);
    Body = body;

    if(numOptional!=0)
    { Defaults = new object[numOptional];
      for(int i=0; i<numOptional; i++) Defaults[i] = parms[i-optionalStart].Default.Evaluate();
    }
  }

  public override object Call(params object[] args) { return DoCall(Template.FixArgs(args, Defaults)); }
  public override object Call(object[] positional, string[] keywords, object[] values)
  { return DoCall(Template.MakeArgs(positional, Defaults, keywords, values));
  }

  public readonly Node Body;
  public readonly object[] Defaults;

  object DoCall(object[] args)
  { Template oldTemplate = Ops.CurrentFunction;
    TopLevel oldTop = TopLevel.Current;
    InterpreterEnvironment oldEnv = InterpreterEnvironment.Current;
    try
    { Ops.CurrentFunction = Template;
      TopLevel.Current = Template.TopLevel;
      if(Template.NumParams!=0)
      { InterpreterEnvironment ne = new InterpreterEnvironment(oldEnv);
        for(int i=0; i<Template.NumParams; i++) ne.Bind(Template.ParamNames[i], args[i]);
        InterpreterEnvironment.Current = ne;
      }
      return Body.Evaluate();
    }
    finally
    { Ops.CurrentFunction = oldTemplate;
      TopLevel.Current = oldTop;
      InterpreterEnvironment.Current = oldEnv;
    }
  }
}
#endregion

#region SimpleProcedure
public abstract class SimpleProcedure : IProcedure
{ public SimpleProcedure(string name, int min, int max) { this.name=name; this.min=min; this.max=max; }

  public int MinArgs { get { return min; } }
  public int MaxArgs { get { return max; } }
  public string Name { get { return name; } }
  public bool NeedsFreshArgs { get { return needsFreshArgs; } }

  public abstract object Call(object[] args);
  public override string ToString() { return name; }

  protected void CheckArity(object[] args) { Ops.CheckArity(name, args.Length, min, max); }

  protected string name;
  protected int min, max;
  protected bool needsFreshArgs;
}
#endregion
#endregion

#region Reference
public sealed class Reference
{ public Reference(object value) { Value = value; }
  public override string ToString() { return "#<reference>"; }
  public object Value;
}
#endregion

#region RG (stuff that can't be written in C#)
public sealed class RG
{ 
  #if !COMPILE_LOWLEVEL
  static RG() { ClosureType = typeof(Closure); }
  #else
  static RG()
  { string dllPath = System.IO.Path.Combine(Scripting.InstallationPath, "Scripting.LowLevel.dll");

    AssemblyGenerator ag = new AssemblyGenerator("Scripting.LowLevel", dllPath, false);
    TypeGenerator tg;
    CodeGenerator cg;

    #region Closure
    { tg = ag.DefineType(TypeAttributes.Public|TypeAttributes.Sealed, "Scripting.Backend.Closure", typeof(ClosureBase));

      #region Constructor(Template, LocalEnvironment)
      cg = tg.DefineConstructor(new Type[] { typeof(Template), typeof(LocalEnvironment) });
      cg.EmitThis();
      cg.EmitArgGet(0);
      cg.EmitFieldSet(typeof(Lambda), "Template");
      cg.EmitThis();
      cg.EmitArgGet(1);
      cg.EmitFieldSet(typeof(ClosureBase), "Environment");
      cg.EmitReturn();
      cg.Finish();
      #endregion

      #region Constructor(Template, LocalEnvironment, object[])
      cg = tg.DefineConstructor(new Type[] { typeof(Template), typeof(LocalEnvironment), typeof(object[]) });
      cg.EmitThis();
      cg.EmitArgGet(0);
      cg.EmitFieldSet(typeof(Lambda), "Template");
      cg.EmitThis();
      cg.EmitArgGet(1);
      cg.EmitFieldSet(typeof(ClosureBase), "Environment");
      cg.EmitThis();
      cg.EmitArgGet(2);
      cg.EmitFieldSet(typeof(ClosureBase), "Defaults");
      cg.EmitReturn();
      cg.Finish();
      #endregion

      #region Call(object[])
      cg = tg.DefineMethodOverride(typeof(Lambda).GetMethod("Call", new Type[] { typeof(object[]) }), true);
      Slot oldFunc = cg.AllocLocalTemp(typeof(Template));
      Slot oldTop  = cg.AllocLocalTemp(typeof(TopLevel));
      cg.EmitFieldGet(typeof(Ops), "CurrentFunction");  // Template oldFunc = Ops.CurrentFunction;
      oldFunc.EmitSet(cg);
      cg.EmitFieldGet(typeof(TopLevel), "Current");     // TopLevel oldTop = TopLevel.Current;
      oldTop.EmitSet(cg);

      cg.ILG.BeginExceptionBlock();                     // try {
      cg.EmitThis();                                    // TopLevel.Current = Template.TopLevel
      cg.EmitFieldGet(typeof(Lambda), "Template");      // Ops.CurrentFunction = Template;
      cg.Dup();
      cg.EmitFieldGet(typeof(Template), "TopLevel");
      oldTop.EmitSet(cg);
      cg.EmitFieldSet(typeof(Ops), "CurrentFunction");

      cg.EmitThis();                                    // return Template.FuncPtr(Environment, Template.FixArgs(
      cg.EmitFieldGet(typeof(ClosureBase), "Environment");  

      cg.EmitThis();
      cg.EmitFieldGet(typeof(Lambda), "Template");
      cg.EmitArgGet(0);                                 //      args,
      cg.EmitThis();                                    //      Defaults))
      cg.EmitFieldGet(typeof(ClosureBase), "Defaults");
      cg.EmitCall(typeof(Template), "FixArgs");

      cg.EmitThis();
      cg.EmitFieldGet(typeof(Lambda), "Template");
      cg.EmitFieldGet(typeof(Template), "FuncPtr");
      cg.ILG.Emit(OpCodes.Tailcall); // TODO: with the addition of the exception block, this now has no effect. see if we can somehow preserve tail calling
      cg.ILG.EmitCalli(OpCodes.Calli, CallingConventions.Standard, typeof(object),
                       new Type[] { typeof(LocalEnvironment), typeof(object[]) }, null);
      cg.EmitReturn();
      cg.ILG.BeginFinallyBlock();                       // } finally {
      oldFunc.EmitGet(cg);                              // Ops.CurrentFunction = oldFunc;
      cg.EmitFieldSet(typeof(Ops), "CurrentFunction");
      oldTop.EmitGet(cg);                               // TopLevel.Current = oldTop;
      cg.EmitFieldSet(typeof(TopLevel), "Current");
      cg.ILG.EndExceptionBlock();                       // }
      cg.FreeLocalTemp(oldFunc);
      cg.FreeLocalTemp(oldTop);
      cg.EmitReturn();
      cg.Finish();
      #endregion

      #region Call(object[], string[], object[])
      MethodInfo baseMethod =
        typeof(Lambda).GetMethod("Call", new Type[] { typeof(object[]), typeof(string[]), typeof(object[]) });
      cg = tg.DefineMethodOverride(baseMethod, true);
      oldFunc = cg.AllocLocalTemp(typeof(Template));
      oldTop  = cg.AllocLocalTemp(typeof(TopLevel));
      cg.EmitFieldGet(typeof(Ops), "CurrentFunction");  // Template oldFunc = Ops.CurrentFunction;
      oldFunc.EmitSet(cg);
      cg.EmitFieldGet(typeof(TopLevel), "Current");     // TopLevel oldTop = TopLevel.Current;
      oldTop.EmitSet(cg);

      cg.ILG.BeginExceptionBlock();                     // try {
      cg.EmitThis();                                    // TopLevel.Current = Template.TopLevel
      cg.EmitFieldGet(typeof(Lambda), "Template");      // Ops.CurrentFunction = Template;
      cg.Dup();
      cg.EmitFieldGet(typeof(Template), "TopLevel");
      oldTop.EmitSet(cg);
      cg.EmitFieldSet(typeof(Ops), "CurrentFunction");

      cg.EmitThis();                                    // return Template.FuncPtr(Environment, Template.MakeArgs(
      cg.EmitFieldGet(typeof(ClosureBase), "Environment");
      cg.EmitThis();
      cg.EmitFieldGet(typeof(Lambda), "Template");
      cg.EmitArgGet(0);                                 //      Positional,
      cg.EmitThis();                                    //      Defaults,
      cg.EmitFieldGet(typeof(ClosureBase), "Defaults");
      cg.EmitArgGet(1);                                 //      Keywords,
      cg.EmitArgGet(2);                                 //      Values))
      cg.EmitCall(typeof(Template), "MakeArgs");

      cg.EmitThis();
      cg.EmitFieldGet(typeof(Lambda), "Template");
      cg.EmitFieldGet(typeof(Template), "FuncPtr");
      cg.ILG.Emit(OpCodes.Tailcall); // TODO: with the addition of the exception block, this now has no effect. see if we can somehow preserve tail calling
      cg.ILG.EmitCalli(OpCodes.Calli, CallingConventions.Standard, typeof(object),
                       new Type[] { typeof(LocalEnvironment), typeof(object[]) }, null);
      cg.EmitReturn();
      cg.ILG.BeginFinallyBlock();                       // } finally {
      oldFunc.EmitGet(cg);                              // Ops.CurrentFunction = oldFunc;
      cg.EmitFieldSet(typeof(Ops), "CurrentFunction");
      oldTop.EmitGet(cg);                               // TopLevel.Current = oldTop;
      cg.EmitFieldSet(typeof(TopLevel), "Current");
      cg.ILG.EndExceptionBlock();                       // }
      cg.FreeLocalTemp(oldFunc);
      cg.FreeLocalTemp(oldTop);
      cg.EmitReturn();
      cg.Finish();
      #endregion

      ClosureType = tg.FinishType();
    }
    #endregion

    try { ag.Save(); } catch { }
  }
  #endif

  public static readonly Type ClosureType;
}
#endregion

#region Template
public sealed class Template
{ public Template(IntPtr func, Language language, string name, string[] paramNames, int numRequired,
                  bool hasList, bool hasDict, bool argsClosed)
  { TopLevel=TopLevel.Current; FuncPtr=func; Language=language; Name=name; ParamNames=paramNames;
    NumRequired=numRequired; NumParams=paramNames.Length; HasList=hasList; HasDict=hasDict; ArgsClosed=argsClosed;
  }

  public object[] FixArgs(object[] args, object[] defaults)
  { int nargs = args.Length;
    if(!HasList)
    { if(nargs==NumParams) return args;
      else if(nargs>NumParams) throw new ArgumentException(Name+": expected at most "+NumParams+
                                                           " positional arguments, but received "+nargs);
    }
    if(nargs<NumRequired) throw new ArgumentException(Name+": expected at least "+NumRequired+
                                                      " positional arguments, but received "+nargs);

    object[] newargs = nargs==NumParams ? args : new object[NumParams];

    int plen=NumParams, pos=plen-(HasList ? 1 : 0)-(HasDict ? 1 : 0), ai=Math.Min(pos, nargs);

    if(HasList) newargs[pos] = Language.PackArguments(args, ai, nargs-ai);
    if(HasDict) newargs[pos+(HasList ? 1 : 0)] = Language.MakeKeywordDict();
    if(args!=newargs) Array.Copy(args, newargs, ai);
    plen = pos-ai;
    if(plen!=0) Array.Copy(defaults, defaults.Length-plen, newargs, ai, plen);
    return newargs;
  }

  public unsafe object[] MakeArgs(object[] positional, object[] defaults, string[] keywords, object[] values)
  { int pi, js, plen=NumParams;
    IDictionary dict = null;
    object[] newargs = new object[plen];
    bool* done = stackalloc bool[plen];

    if(positional==null || positional.Length==0)
    { for(int i=0; i<plen; i++) done[i] = false;
      pi = js = 0;
    }
    else
    { pi = js = Math.Min(plen-(HasList ? 1 : 0)-(HasDict ? 1 : 0), positional.Length);
      Array.Copy(positional, newargs, pi);
      int i=0;
      for(; i<pi; i++) done[i] = true;
      for(; i<plen; i++) done[i] = false;
    }

    for(int i=0; i<keywords.Length; i++)
    { string name = keywords[i];
      for(int j=js; j<plen; j++)
        if(ParamNames[j]==name)
        { if(done[j]) throw new TargetException(Name+": got duplicate values for parameter '"+name+"'");
          newargs[j] = values[i];
          done[j] = true;
          if(j==js) do js++; while(js<plen && done[js]);
          goto next;
        }
      for(int j=0; j<js; j++)
        if(ParamNames[j]==name) throw new TargetException(Name+": got duplicate values for parameter '"+name+"'");
      if(HasDict)
      { if(dict==null) dict = Language.MakeKeywordDict();
        dict[name] = values[i];
      }
      else throw new TargetException(Name+": got an unexpected keyword parameter '"+name+"'");
      next:;
    }
    
    if(HasDict)
    { if(done[--plen])
      { if(dict!=null) throw new TargetException(Name+": got duplicate values for parameter '"+ParamNames[plen]+"'");
      }
      else newargs[plen] = dict==null ? Language.MakeKeywordDict() : dict;
    }
    
    if(HasList) newargs[--plen] = Language.PackArguments(positional, pi, positional.Length-pi);
    
    for(; pi<plen; pi++) if(!done[pi]) newargs[pi] = defaults[pi-NumRequired];
    for(pi=0; pi<NumRequired; pi++)
      if(!done[pi]) throw new TargetException(Name+": no value given for parameter '"+ParamNames[pi]+"'");

    return newargs;
  }

  public readonly TopLevel TopLevel;
  public readonly Language Language;
  public readonly string Name;
  public readonly string[] ParamNames;
  public readonly IntPtr FuncPtr;
  public readonly int NumParams, NumRequired;
  public readonly bool HasList, HasDict, ArgsClosed;
}
#endregion

} // namespace Scripting.Backend