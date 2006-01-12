/*
Scripting is a low level framework for building dynamic languages.
It produces languages which can be interpreted or compiled, targetting
the Microsoft .NET Framework.

http://www.adammil.net/
Copyright (C) 2005 Adam Milazzo

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
using System.Collections.Specialized;
using System.Globalization;
using System.Reflection;
using System.Reflection.Emit;

namespace Scripting
{

// FIXME: if a lambda accesses top-level environment (by calling EVAL), it will get the TL of the caller,
//        not of where it was defined
// FIXME: implement tail-call elimination in interpreted mode
#region Procedures
public interface IProcedure
{ int MinArgs { get; }
  int MaxArgs { get; }
  bool NeedsFreshArgs { get; }

  object Call(params object[] args);
}

public interface IFancyProcedure : IProcedure
{ object Call(object[] args, string[] keywords, object[] values);
}

public abstract class Lambda : IFancyProcedure
{ public int MinArgs { get { return Template.NumParams; } }
  public int MaxArgs { get { return Template.HasList ? -1 : Template.NumParams; } }
  public bool NeedsFreshArgs { get { return Template.ArgsClosed; } }

  public abstract object Call(params object[] args);
  public abstract object Call(object[] positional, string[] keywords, object[] values);

  public override string ToString() { return Template.Name==null ? "#<lambda>" : "#<lambda '"+Template.Name+"'>"; }

  public Template Template;
}

public abstract class Closure : Lambda
{ public LocalEnvironment Environment;
  public object[] Defaults;
}

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
  { TopLevel oldt = TopLevel.Current;
    if(Template.NumParams==0)
      try
      { TopLevel.Current = Template.TopLevel;
        return Body.Evaluate();
      }
      finally { TopLevel.Current = oldt; }
    else
    { InterpreterEnvironment ne, oldi=InterpreterEnvironment.Current;
      try
      { InterpreterEnvironment.Current = ne = new InterpreterEnvironment(oldi);
        TopLevel.Current = Template.TopLevel;
        for(int i=0; i<Template.NumParams; i++) ne.Bind(Template.ParamNames[i], args[i]);
        return Body.Evaluate();
      }
      finally
      { InterpreterEnvironment.Current = oldi;
        TopLevel.Current = oldt;
      }
    }
  }
}

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

public abstract class Primitive : SimpleProcedure
{ public Primitive(string name, int min, int max) : base(name, min, max) { }
  public override string ToString() { return string.Format("#<primitive procedure '{0}'>", name); }
}
#endregion

#region Enums
[Flags]
public enum Conversion
{ None=0,
  // 1/3 and 8/12 are chosen to make it clear that those are mutually exclusive
  Unsafe=1, Safe=2, Reference=3, Identity=4, UnsafeAPA=8, SafeAPA=16, RefAPA=24, PacksPA=32,
  QualityMask=31
}
#endregion

#region Binding
public sealed class Binding
{ public Binding(string name, TopLevel env) { Value=Unbound; Name=name; Environment=env; }
  public Binding(string name, object value, TopLevel env) { Value=value; Name=name; Environment=env; }

  public override bool Equals(object obj) { return this==obj; }
  public override int GetHashCode() { return Name.GetHashCode(); }

  public object Value;
  public string Name;
  public TopLevel Environment;

  public readonly static object Unbound = new Singleton("<UNBOUND>");
}
#endregion

#region Cast
public sealed class Cast : MemberContainer
{ public Cast(ReflectedType type, object value)
  { ReflectedType=type; Type=type.Type; Value=value;
    if(value!=null && !Type.IsAssignableFrom(value.GetType()))
      throw new InvalidCastException("Attempted to cast "+Ops.TypeName(value)+" to "+Ops.TypeName(Type));
  }

  public override void DeleteSlot(string name) { ReflectedType.DeleteSlot(name); }

  public override object GetSlot(string name) { return ReflectedType.GetSlot(name); }
  public override bool GetSlot(string name, out object ret) { return ReflectedType.GetSlot(name, out ret); }
  public override object GetValue(string name, params object[] args) { return ReflectedType.GetValue(name, args); }
  public override bool GetValue(string name, out object value, params object[] args)
  { return ReflectedType.GetValue(name, out value, args);
  }

  public override ICollection GetMemberNames(bool includeImports)
  { return ReflectedType.GetMemberNames(includeImports);
  }

  public override void Import(TopLevel top, string[] names, string[] asNames)
  { ReflectedType.Import(top, names, asNames);
  }

  public override void SetSlot(string name, object value) { ReflectedType.SetSlot(name, value); }
  public override void SetValue(string name, object[] args, object value)
  { ReflectedType.SetValue(name, args, value);
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

  public override void DeleteSlot(string name) { TopLevel.Globals.Unbind(name); }

  public override object GetSlot(string name) { return TopLevel.Globals.Get(name); }
  public override bool GetSlot(string name, out object ret) { return TopLevel.Globals.Get(name, out ret); }

  public override ICollection GetMemberNames(bool includeImports)
  { if(includeImports) return TopLevel.Globals.Dict.Keys;

    ArrayList ret = new ArrayList(Math.Max(TopLevel.Globals.Dict.Count/2, 16));
    foreach(DictionaryEntry de in TopLevel.Globals.Dict)
    { Binding bind = (Binding)de.Value;
      if(bind.Environment==TopLevel) ret.Add(de.Key);
    }
    return ret;
  }

  public override void Import(TopLevel top, string[] names, string[] asNames)
  { if(names==null)
    { Import(top.Globals, TopLevel.Globals, TopLevel);
      Import(top.Macros, TopLevel.Macros, TopLevel);
    }
    else
      for(int i=0; i<names.Length; i++)
      { object ret;
        bool found = false;
        if(TopLevel.Globals.Get(names[i], out ret))
        { top.Globals.Bind(asNames[i], ret, TopLevel);
          found = true;
        }
        if(TopLevel.Macros.Get(names[i], out ret))
        { top.Macros.Bind(asNames[i], ret, TopLevel);
          found = true;
        }
        if(!found) throw new ArgumentException("'"+names[i]+"' not found in module "+Name);
      }
  }

  public override void SetSlot(string name, object value) { TopLevel.Set(name, value); }

  public override string ToString() { return "#<module '"+Name+"'>"; }

  public readonly TopLevel TopLevel;
  public readonly string Name;

  static void Import(BindingSpace to, BindingSpace from, TopLevel env)
  { Language lang = Options.Current.Language;
    foreach(DictionaryEntry de in from.Dict)
    { string key = (string)de.Key;
      if(!lang.ExcludeFromImport(key))
      { Binding bind = (Binding)de.Value;
        if(bind.Environment==env) to.Bind(key, bind.Value, env);
      }
    }
  }
}
#endregion

#region Environments
#region BindingSpace
public sealed class BindingSpace
{ public void Alter(string name, object value)
  { Binding bind;
    lock(Dict) bind = (Binding)Dict[name];
    if(bind==null) throw new NameException("no such name: "+name);
    bind.Value = value;
  }

  public void Bind(string name, object value, TopLevel env)
  { Binding bind;
    lock(Dict)
    { bind = (Binding)Dict[name];
      if(bind==null || bind.Environment!=env) Dict[name] = bind = new Binding(name, env);
    }
    bind.Value = value;
  }

  public bool Contains(string name)
  { Binding bind;
    lock(Dict) bind = (Binding)Dict[name];
    return bind!=null && bind.Value!=Binding.Unbound;
  }

  public object Get(string name)
  { Binding bind;
    lock(Dict) bind = (Binding)Dict[name];
    if(bind==null || bind.Value==Binding.Unbound) throw new NameException("no such name: "+name);
    return bind.Value;
  }

  public bool Get(string name, out object value)
  { Binding bind;
    lock(Dict) bind = (Binding)Dict[name];
    if(bind==null || bind.Value==Binding.Unbound) { value=null; return false; }
    value = bind.Value;
    return true;
  }

  public Binding GetBinding(string name, TopLevel env)
  { Binding bind;
    lock(Dict) bind = (Binding)Dict[name];
    if(bind==null) Dict[name] = bind = new Binding(name, env);
    return bind;
  }

  public void Set(string name, object value, TopLevel env)
  { Binding bind;
    lock(Dict)
    { bind = (Binding)Dict[name];
      if(bind==null) Dict[name] = bind = new Binding(name, env);
      else bind.Environment = env;
    }
    bind.Value = value;
  }

  public void Unbind(string name) { lock(Dict) Dict.Remove(name); }

  public readonly Hashtable Dict = new Hashtable();
}
#endregion

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

public sealed class LocalEnvironment
{ public LocalEnvironment(LocalEnvironment parent, object[] values) { Parent=parent; Values=values; }
  public LocalEnvironment(LocalEnvironment parent, int length) { Parent=parent; Values=new object[length]; }
  public LocalEnvironment(LocalEnvironment parent, object[] values, int length)
  { Parent=parent; Values=new object[length];
    Array.Copy(values, Values, values.Length);
  }

  public readonly LocalEnvironment Parent;
  public readonly object[] Values;
}

public sealed class TopLevel
{ public void AddMacro(string name, IProcedure value) { Macros.Set(name, value, this); }

  public void Alter(string name, object value) { Globals.Alter(name, value); }
  public void Bind(string name, object value) { Globals.Bind(name, value, this); }

  public bool Contains(string name) { return Globals.Contains(name); }
  public bool ContainsMacro(string name) { return Macros.Contains(name); }

  public object Get(string name) { return Globals.Get(name); }
  public bool Get(string name, out object value) { return Globals.Get(name, out value); }
  public Binding GetBinding(string name) { return Globals.GetBinding(name, this); }
  public IProcedure GetMacro(string name) { return (IProcedure)Macros.Get(name); }

  public void Set(string name, object value) { Globals.Set(name, value, this); }
  public void Unbind(string name) { Globals.Unbind(name); }

  public BindingSpace Globals=new BindingSpace(), Macros=new BindingSpace();

  [ThreadStatic] public static TopLevel Current;
}
#endregion

#region IProperty
public interface IProperty
{ object Get(object[] args);
  object Set(object[] args, object value);
}
#endregion

#region MemberCache
public struct MemberCache
{ public object Type, Value;

  public static object TypeFromObject(object obj)
  { MemberContainer mc = obj as MemberContainer;
    if(mc==null) return obj.GetType();
    ReflectedType rt = obj as ReflectedType;
    return rt==null ? mc : (object)rt.Type;
  }
}
#endregion

#region MemberContainer
public abstract class MemberContainer
{ public abstract void DeleteSlot(string name);

  public ICollection GetMemberNames() { return GetMemberNames(false); }
  public abstract ICollection GetMemberNames(bool includeImports);

  public abstract object GetSlot(string name);
  public abstract bool GetSlot(string name, out object ret);

  public object GetValue(string name) { return GetValue(name, Ops.EmptyArray); }
  public virtual object GetValue(string name, params object[] args)
  { return Ops.GetPropertyValue(GetSlot(name), args);
  }

  public bool GetValue(string name, out object value) { return GetValue(name, out value, Ops.EmptyArray); }
  public virtual bool GetValue(string name, out object value, params object[] args)
  { if(GetSlot(name, out value)) { value=Ops.GetPropertyValue(value, args); return true; }
    else return false;
  }

  public void Import(TopLevel top) { Import(top, null, null); }
  public void Import(TopLevel top, string[] names) { Import(top, names, names); }
  public abstract void Import(TopLevel top, string[] names, string[] asNames);

  public abstract void SetSlot(string name, object value);

  public void SetValue(string name, object value) { SetValue(name, Ops.EmptyArray, value); }
  public virtual void SetValue(string name, object[] args, object value)
  { IProperty prop = GetSlot(name) as IProperty;
    if(prop!=null) prop.Set(args, value);
    else SetSlot(name, value);
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
      sb.Append(Ops.Repr(Values[i]));
    }
    return sb.Append('}').ToString();
  }

  public object[] Values;
}
#endregion

#region Ops
public sealed class Ops
{ Ops() { }

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

  public static object Call(string name) { return Call(GetGlobal(name), EmptyArray); }
  public static object Call(string name, params object[] args) { return Call(GetGlobal(name), args); }
  public static object Call(object func) { return ExpectProcedure(func).Call(EmptyArray); }
  public static object Call(object func, params object[] args) { return ExpectProcedure(func).Call(args); }

  public static object Call(object func, CallArg[] args)
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

    object[] positional = num==0 ? null : new object[num];
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

    if(ai==args.Length) return ExpectProcedure(func).Call(positional);

    num = 0;
    for(int i=ai; i<args.Length; i++)
      if(args[i].Type==CallArg.DictType)
      { IDictionary dict = args[i].Value as IDictionary;
        if(dict!=null) num += dict.Count;
        else throw new ArgumentException("Expected IDictionary dict type, but received "+TypeName(args[i].Value));
        hasdict = true;
      }
      else num += ((object[])args[i].Type).Length;

    if(!hasdict)
      return ExpectFancyProcedure(func).Call(positional, (string[])args[ai].Value, (object[])args[ai].Type);

    string[] names = new string[num];
    object[] values = new object[num];
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

    return ExpectFancyProcedure(func).Call(positional, names, values);
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
  { if(bind.Value==Binding.Unbound) throw new NameException("use of unbound variable: "+bind.Name);
    return bind;
  }

  public static void CheckVariable(object value, string name)
  { if(value==Binding.Unbound) throw new NameException("use of unbound variable: "+name);
  }

  public static object[] CheckValues(MultipleValues values, int length)
  { if(values.Values.Length<length) throw Ops.ValueError("expected at least "+length.ToString()+
                                                         " values, but received "+values.Values.Length.ToString());
    return values.Values;
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

  public static object ConvertTo(object o, Type type)
  { switch(ConvertTo(o==null ? null : o.GetType(), type))
    { case Conversion.Identity: case Conversion.Reference: return o;
      case Conversion.None: throw TypeError("object cannot be converted to '{0}'", type);
      default:
        if(type==typeof(bool)) return FromBool(IsTrue(o));
        if(type.IsSubclassOf(typeof(Delegate))) return MakeDelegate(o, type);
        try { return Convert.ChangeType(o, type); }
        catch(OverflowException) { throw ValueError("large value caused overflow"); }
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
    if(to.IsSubclassOf(typeof(Delegate)))
      return typeof(IProcedure).IsAssignableFrom(from) ? Conversion.Unsafe : Conversion.None;
    return Conversion.None;
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
    if(e==null) throw Ops.TypeError("expected enumerable object but received "+Ops.TypeName(obj));
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
  { IProcedure ret = obj as IProcedure;
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
    if(type!=null) return ReflectedType.FromType(type);
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

  // TODO: check whether we can eliminate this (ie, "(eq? #t #t)" still works reliably)
  public static object FromBool(bool value) { return value ? TRUE : FALSE; }

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
    throw new ArgumentException("objects of type '"+TypeName(col)+"' are not indexable");
  }

  public static void SetIndex(object value, object col, object index)
  { IList list = col as IList;
    if(list!=null) list[ToInt(index)] = value;
    else
    { IDictionary dict = col as IDictionary;
      if(dict!=null) dict[index] = value;
      else throw new ArgumentException("objects of type '"+TypeName(col)+"' are not indexable");
    }
  }

  public static ICollection GetMemberNames(object obj) { return GetMemberNames(obj, false); }
  public static ICollection GetMemberNames(object obj, bool includeImports)
  { return MemberContainer.FromObject(obj).GetMemberNames(includeImports);
  }

  public static object GetProperty(object obj, string name)
  { MemberContainer mc = obj as MemberContainer;
    return mc!=null ? mc.GetValue(name) : MemberContainer.FromObject(obj).GetValue(name, obj);
  }

  public static bool GetProperty(object obj, string name, out object value)
  { MemberContainer mc = obj as MemberContainer;
    return mc!=null ? mc.GetValue(name, out value) : MemberContainer.FromObject(obj).GetValue(name, out value, obj);
  }

  public static object GetPropertyValue(object value) { return GetPropertyValue(value, EmptyArray); }
  public static object GetPropertyValue(object value, params object[] args)
  { IProperty prop = value as IProperty;
    return prop==null ? value : prop.Get(args);
  }

  public static object GetSlot(object obj, string name) { return MemberContainer.FromObject(obj).GetSlot(name); }

  public static bool GetSlot(object obj, string name, out object value)
  { return MemberContainer.FromObject(obj).GetSlot(name, out value);
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
    return GetCurrentLanguage().RightShift(a, b);
  }

  public static object Less(object a, object b) { return FromBool(Compare(a,b)<0); }
  public static object LessEqual(object a, object b) { return FromBool(Compare(a,b)<=0); }

  public static Delegate MakeDelegate(object callable, Type delegateType)
  { IProcedure proc = callable as IProcedure;
    if(proc==null) throw new ArgumentException("delegate: expected a procedure");
    return Delegate.CreateDelegate(delegateType, Interop.MakeDelegateWrapper(proc, delegateType), "Handle");
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
    return GetCurrentLanguage().Add(a, b);
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

  public static string Repr(object obj) { return Options.Current.Language.Repr(obj); }

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

  public static void SetMember(object obj, string name, object value)
  { MemberContainer.FromObject(obj).SetSlot(name, value);
  }

  public static string Str(object obj) { return Options.Current.Language.Str(obj); }

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

  public static double ToFloat(object o)
  { if(o is double) return (double)o;
    if(o is Complex)
    { Complex c = (Complex)o;
      if(c.imag==0) return c.real;
    }

    try { return Convert.ToDouble(o); }
    catch(FormatException) { throw ValueError("string does not contain a valid float"); }
    catch(OverflowException) { throw ValueError("too big for float"); }
    catch(InvalidCastException) { throw TypeError("expected float, but got {0}", TypeName(o)); }
  }

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

  public static int ToInt(object o)
  { if(o is int) return (int)o;

    try
    { switch(Convert.GetTypeCode(o))
      { case TypeCode.Boolean: return (bool)o ? 1 : 0;
        case TypeCode.Byte: return (byte)o;
        case TypeCode.Char: return (char)o;
        case TypeCode.Decimal: return (int)(Decimal)o;
        case TypeCode.Double: return checked((int)(double)o);
        case TypeCode.Int16: return (short)o;
        case TypeCode.Int64: return checked((int)(long)o);
        case TypeCode.SByte: return (sbyte)o;
        case TypeCode.Single: return checked((int)(float)o);
        case TypeCode.String:
          try { return int.Parse((string)o); }
          catch(FormatException) { throw ValueError("string does not contain a valid int"); }
        case TypeCode.UInt16: return (int)(ushort)o;
        case TypeCode.UInt32: return checked((int)(uint)o);
        case TypeCode.UInt64: return checked((int)(ulong)o);
        default: return checked((int)Convert.ToSingle(o)); // we do it this way so it truncates
      }
    }
    catch(FormatException) { throw ValueError("string does not contain a valid int"); }
    catch(OverflowException) { goto toobig; }
    catch(InvalidCastException) { throw TypeError("expected int, but got {0}", TypeName(o)); }
    toobig: throw ValueError("too big for int");
  }

  public static long ToLong(object o)
  { try
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
        case TypeCode.String:
          try { return long.Parse((string)o); }
          catch(FormatException) { throw ValueError("string does not contain a valid long"); }
        case TypeCode.UInt16: return (long)(ushort)o;
        case TypeCode.UInt32: return (long)(uint)o;
        case TypeCode.UInt64: return checked((long)(ulong)o);
        default: return checked((long)Convert.ToSingle(o));
      }
    }
    catch(FormatException) { throw ValueError("string does not contain a valid long"); }
    catch(OverflowException) { throw ValueError("too big for long"); } // TODO: allow conversion to long integer?
    catch(InvalidCastException) { throw TypeError("expected long, but got {0}", TypeName(o)); }
  }

  public static TypeErrorException TypeError(string message) { return new TypeErrorException(message); }
  public static TypeErrorException TypeError(string format, params object[] args)
  { return new TypeErrorException(string.Format(format, args));
  }

  public static string TypeName(object o) { return TypeName(o==null ? null : o.GetType()); }
  public static string TypeName(ReflectedType type) { return TypeName(type.Type); }
  public static string TypeName(Type type) { return Options.Current.Language.TypeName(type); }

  public static ValueErrorException ValueError(string message)
  { return new ValueErrorException(message);
  }
  public static ValueErrorException ValueError(string format, params object[] args)
  { return new ValueErrorException(string.Format(format, args));
  }

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
}
#endregion

#region Reference
public sealed class Reference
{ public Reference(object value) { Value=value; }
  public override string ToString() { return "#<reference>"; }
  public object Value;
}
#endregion

#region RG (stuff that can't be written in C#)
public sealed class RG
{ static RG()
  { if(System.IO.File.Exists(ModuleGenerator.CachePath+"Scripting.LowLevel.dll")) 
      try
      { Assembly ass = Assembly.LoadFrom(ModuleGenerator.CachePath+"Scripting.LowLevel.dll");
        ClosureType = ass.GetType("Scripting.ClosureF", true);
        return;
      }
      catch { }

    AssemblyGenerator ag = new AssemblyGenerator("Scripting.LowLevel",
                                                 ModuleGenerator.CachePath+"Scripting.LowLevel.dll", false);
    TypeGenerator tg;
    CodeGenerator cg;

    #region Closure
    { tg = ag.DefineType(TypeAttributes.Public|TypeAttributes.Sealed, "Scripting.ClosureF", typeof(Closure));
      
      // constructor 1
      cg = tg.DefineConstructor(new Type[] { typeof(Template), typeof(LocalEnvironment) });
      cg.EmitThis();
      cg.EmitArgGet(0);
      cg.EmitFieldSet(typeof(Lambda), "Template");
      cg.EmitThis();
      cg.EmitArgGet(1);
      cg.EmitFieldSet(typeof(Closure), "Environment");
      cg.EmitReturn();
      cg.Finish();

      // constructor 2
      cg = tg.DefineConstructor(new Type[] { typeof(Template), typeof(LocalEnvironment), typeof(object[]) });
      cg.EmitThis();
      cg.EmitArgGet(0);
      cg.EmitFieldSet(typeof(Lambda), "Template");
      cg.EmitThis();
      cg.EmitArgGet(1);
      cg.EmitFieldSet(typeof(Closure), "Environment");
      cg.EmitThis();
      cg.EmitArgGet(2);
      cg.EmitFieldSet(typeof(Closure), "Defaults");
      cg.EmitReturn();
      cg.Finish();

      // Call(object[])
      cg = tg.DefineMethodOverride(typeof(Lambda).GetMethod("Call", new Type[] { typeof(object[]) }), true);
      Slot oldFunc = cg.AllocLocalTemp(typeof(Template));
      cg.EmitFieldGet(typeof(Ops), "CurrentFunction");
      oldFunc.EmitSet(cg);

      cg.EmitThis();
      cg.EmitFieldGet(typeof(Lambda), "Template");
      cg.EmitFieldSet(typeof(Ops), "CurrentFunction");

      cg.ILG.BeginExceptionBlock();
      cg.EmitThis();
      cg.EmitFieldGet(typeof(Closure), "Environment");

      cg.EmitThis();
      cg.EmitFieldGet(typeof(Lambda), "Template");
      cg.EmitArgGet(0);
      cg.EmitThis();
      cg.EmitFieldGet(typeof(Closure), "Defaults");
      cg.EmitCall(typeof(Template), "FixArgs");

      cg.EmitThis();
      cg.EmitFieldGet(typeof(Lambda), "Template");
      cg.EmitFieldGet(typeof(Template), "FuncPtr");
      cg.ILG.Emit(OpCodes.Tailcall); // TODO: with the addition of the exception block, this now has no effect. see if we can somehow preserve tail calling
      cg.ILG.EmitCalli(OpCodes.Calli, CallingConventions.Standard, typeof(object),
                       new Type[] { typeof(LocalEnvironment), typeof(object[]) }, null);
      cg.EmitReturn();
      cg.ILG.BeginFinallyBlock();
      oldFunc.EmitGet(cg);
      cg.EmitFieldSet(typeof(Ops), "CurrentFunction");
      cg.ILG.EndExceptionBlock();
      cg.FreeLocalTemp(oldFunc);
      cg.EmitReturn();
      cg.Finish();

      // Call(object[], string[], object[])
      MethodInfo baseMethod =
        typeof(Lambda).GetMethod("Call", new Type[] { typeof(object[]), typeof(string[]), typeof(object[]) });
      cg = tg.DefineMethodOverride(baseMethod, true);
      oldFunc = cg.AllocLocalTemp(typeof(Template));
      cg.EmitFieldGet(typeof(Ops), "CurrentFunction");
      oldFunc.EmitSet(cg);

      cg.EmitThis();
      cg.EmitFieldGet(typeof(Lambda), "Template");
      cg.EmitFieldSet(typeof(Ops), "CurrentFunction");

      cg.ILG.BeginExceptionBlock();
      cg.EmitThis();
      cg.EmitFieldGet(typeof(Closure), "Environment");

      cg.EmitThis();
      cg.EmitFieldGet(typeof(Lambda), "Template");
      cg.EmitArgGet(0);
      cg.EmitThis();
      cg.EmitFieldGet(typeof(Closure), "Defaults");
      cg.EmitArgGet(1);
      cg.EmitArgGet(2);
      cg.EmitCall(typeof(Template), "MakeArgs");

      cg.EmitThis();
      cg.EmitFieldGet(typeof(Lambda), "Template");
      cg.EmitFieldGet(typeof(Template), "FuncPtr");
      cg.ILG.Emit(OpCodes.Tailcall); // TODO: with the addition of the exception block, this now has no effect. see if we can somehow preserve tail calling
      cg.ILG.EmitCalli(OpCodes.Calli, CallingConventions.Standard, typeof(object),
                       new Type[] { typeof(LocalEnvironment), typeof(object[]) }, null);
      cg.EmitReturn();
      cg.ILG.BeginFinallyBlock();
      oldFunc.EmitGet(cg);
      cg.EmitFieldSet(typeof(Ops), "CurrentFunction");
      cg.ILG.EndExceptionBlock();
      cg.FreeLocalTemp(oldFunc);
      cg.EmitReturn();
      cg.Finish();

      ClosureType = tg.FinishType();
    }
    #endregion

    try { ag.Save(); } catch { }
  }

  public static readonly Type ClosureType;
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
      else if(nargs>NumParams) throw new TargetParameterCountException(Name+": expected at most "+NumParams+
                                                                       " positional arguments, but received "+nargs);
    }
    if(nargs<NumRequired) throw new TargetParameterCountException(Name+": expected at least "+NumRequired+
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

#region Void
public sealed class Void
{ Void() { }
  public static readonly Void Value = new Void();
}
#endregion

} // namespace Scripting