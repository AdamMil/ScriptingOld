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
using System.Collections.Specialized;
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

namespace Scripting
{

#region FieldWrapper
public abstract class FieldWrapper : SimpleProcedure
{ public FieldWrapper(string name, bool isStatic) : base("#<field '"+name+"'>", isStatic ? 0 : 1, isStatic ? 1 : 2) { }
}
#endregion

#region FunctionWrapper
public abstract class FunctionWrapper : IProcedure, IProperty
{ public abstract int MinArgs { get; }
  public abstract int MaxArgs { get; }
  public bool NeedsFreshArgs { get { return false; } }

  public abstract object Call(object[] args);
  public abstract Conversion TryMatch(object[] args, Type[] types);

  #region IProperty Members
  public bool Call(object instance, out object ret, params object[] args) { ret=Call(args); return true; }

  public bool Call(object instance, out object ret, object[] positional, string[] names, object[] values)
  { throw new NotImplementedException("fancy calling .NET methods is not implemented");
  }

  public bool Get(object instance, out object ret)
  { ret = isStatic || instance==null ? (IProcedure)this : new InstanceWrapper(instance, this);
    return true;
  }

  public bool GetAccessor(object instance, out IProcedure ret) { ret=this; return true; }
  public bool TrySet(object instance, object value) { return false; }
  #endregion

  protected bool isStatic;

  // TODO: see if we can optimize this even further?
  protected static Conversion TryMatch(object[] args, Type[] types, Type[] ptypes, int numNP, int min, bool paramArray)
  { if(args.Length<min || !paramArray && args.Length>ptypes.Length) return Conversion.None; // check number of required parameters
    Conversion ret=Conversion.Identity;

    // check types of all normal (non-paramarray) parameters
    for(int i=0; i<numNP; i++)
    { Conversion conv = Ops.ConvertTo(types[i], ptypes[i]);
      if(conv==Conversion.None) return Conversion.None;
      if(conv<ret) ret=conv;
    }

    if(paramArray)
    { Type etype = ptypes[numNP].GetElementType();

      if(args.Length==ptypes.Length && types[numNP].IsArray)
      { Conversion conv = Ops.ConvertTo(types[numNP], ptypes[numNP]);
        if(conv==Conversion.Identity || conv==Conversion.Reference)
        { if(conv<ret) ret = conv;
          return ret | Conversion.RefAPA;
        }

        conv = Ops.ConvertTo(types[numNP], etype);
        if(conv==Conversion.Identity || conv==Conversion.Reference)
          return (conv<ret ? conv : ret) | Conversion.PacksPA;

        // otherwise check that the remaining arguments can be converted to the element type
        conv = Ops.ConvertTo(types[numNP].GetElementType(), etype);
        if(conv==Conversion.None) return Conversion.None;
        if(conv<ret) ret=conv;
        return ret | (conv==Conversion.Unsafe ? Conversion.UnsafeAPA : Conversion.SafeAPA);
      }

      // check if extra parameters can be converted to the element type
      for(int i=numNP; i<args.Length; i++)
      { Conversion conv = Ops.ConvertTo(types[i], etype);
        if(conv==Conversion.None) return Conversion.None;
        if(conv<ret) ret=conv;
      }
      ret |= Conversion.PacksPA;
    }

    return ret;
  }
}

public abstract class FunctionWrapperI : FunctionWrapper
{ public FunctionWrapperI(IntPtr method, bool isStatic) { methodPtr=method; this.isStatic=isStatic; }
  protected readonly IntPtr methodPtr;
}
#endregion

#region Interop
public sealed class Interop
{ Interop() { }

  #region Call
  public static object Call(FunctionWrapper[] funcs, object[] args)
  { if(funcs.Length==1) return funcs[0].Call(args);

    Conversion best=Conversion.None, bqual=Conversion.None;
    int besti=-1;

    Type[] types = new Type[args.Length];
    for(int i=0; i<args.Length; i++)
    { object o = args[i];
      if(o==null) types[i] = null;
      else
      { Type type = o.GetType();
        if(type==Cast.ClassType)
        { Cast cast = (Cast)o;
          args[i]  = cast.Value;
          types[i] = cast.Type;
        }
        else types[i] = type;
      }
    }

    unsafe
    { Conversion *rets = stackalloc Conversion[funcs.Length];

      for(int i=0; i<funcs.Length; i++)
      { Conversion conv = funcs[i].TryMatch(args, types), qual=conv&Conversion.QualityMask;
        if(qual>bqual || qual==bqual && (conv>best && (best&Conversion.PacksPA)!=0 ||
                                         conv<best && (conv&Conversion.PacksPA)==0))
        { best=conv; bqual=qual; besti=i;
          if(bqual==Conversion.Identity) break; // this complements the check down below
        }
        rets[i] = conv;
      }

      if(besti==-1) throw new ArgumentException("Unable to bind arguments");
      if(bqual!=Conversion.Identity)
        for(int i=besti+1; i<funcs.Length; i++)
          if(rets[i]==best) throw new ArgumentException("Ambiguous arguments");
    }

    return funcs[besti].Call(args);
  }
  #endregion

  public static Type GetType(string name) { return GetType(name, false); }
  public static Type GetType(string name, bool throwOnError)
  { Type type = Type.GetType(name);
    if(type==null)
      foreach(Assembly a in AppDomain.CurrentDomain.GetAssemblies())
      { type = a.GetType(name);
        if(type!=null) break;
      }
    if(type==null && throwOnError) throw new ArgumentException("Unable to load type: "+name, "name");
    return type;
  }

  static void LoadAssembly(Assembly ass)
  { Hashtable namespaces = new Hashtable();
    foreach(Type type in ass.GetTypes())
    { if(!type.IsPublic || type.DeclaringType!=null || type.Namespace==null) continue;
      ArrayList list = (ArrayList)namespaces[type.Namespace];
      if(list==null) namespaces[type.Namespace] = list = new ArrayList();
      list.Add(type);
    }
    foreach(DictionaryEntry de in namespaces)
    { ReflectedNamespace ns = ReflectedNamespace.FromName((string)de.Key);
      foreach(Type type in (ArrayList)de.Value) ns.AddType(type);
    }
  }

  public static void LoadAssemblyByName(string name)
  { Assembly ass = Assembly.LoadWithPartialName(name);
    if(ass==null) throw new ArgumentException("Assembly "+name+" could not be loaded");
    LoadAssembly(ass);
  }

  public static void LoadAssemblyFromFile(string name) { LoadAssembly(Assembly.LoadFrom(name)); }

  #region MakeDelegateWrapper
  public static object MakeDelegateWrapper(IProcedure proc, Type delegateType)
  { Create cr;
    lock(handlers) cr = (Create)handlers[delegateType];

    if(cr==null)
    { MethodInfo mi = delegateType.GetMethod("Invoke", BindingFlags.Public|BindingFlags.Instance);
      Signature sig = new Signature(mi, true);

      lock(dsigs)
      { cr = (Create)dsigs[sig];
        if(cr==null)
        { for(int i=0; i<sig.Params.Length; i++)
            if(sig.Params[i].IsByRef) throw new NotImplementedException(); // TODO: implement this
          TypeGenerator tg = SnippetMaker.Assembly.DefineType(TypeAttributes.Public|TypeAttributes.Sealed,
                                                              "dw$"+dwi.Next, null);
          Slot pslot = tg.DefineField(FieldAttributes.Private, "proc", typeof(IProcedure));

          CodeGenerator cg = tg.DefineConstructor(typeof(IProcedure));
          cg.EmitArgGet(0);
          pslot.EmitSet(cg);
          cg.EmitReturn();
          cg.Finish();
          ConstructorInfo cons = (ConstructorInfo)cg.MethodBase;

          cg = tg.DefineMethod("Handle", sig.Return, sig.Params);
          pslot.EmitGet(cg);
          if(sig.Params.Length==0) cg.EmitFieldGet(typeof(Ops), "EmptyArray");
          else
          { cg.EmitNewArray(typeof(object), sig.Params.Length);
            for(int i=0; i<sig.Params.Length; i++)
            { cg.Dup();
              cg.EmitInt(i);
              cg.EmitArgGet(i);
              if(sig.Params[i].IsValueType) cg.ILG.Emit(OpCodes.Box, sig.Params[i]);
              cg.ILG.Emit(OpCodes.Stelem_Ref);
            }
            if(sig.Return==typeof(object)) cg.ILG.Emit(OpCodes.Tailcall);
            cg.EmitCall(typeof(IProcedure), "Call");
            if(sig.Return==typeof(void)) cg.ILG.Emit(OpCodes.Pop);
            else EmitConvertTo(cg, sig.Return);
            cg.EmitReturn();
            cg.Finish();
          }

          cg = tg.DefineStaticMethod("Create", typeof(object), typeof(IProcedure));
          cg.EmitArgGet(0);
          cg.EmitNew(cons);
          cg.EmitReturn();
          cg.Finish();

          Type type = tg.FinishType();
          dsigs[sig] = cr = (Create)Delegate.CreateDelegate(typeof(Create), type.GetMethod("Create"));
        }
      }

      lock(handlers) handlers[delegateType] = cr;
    }

    return cr(proc);
  }
  #endregion

  internal static void EmitConvertTo(CodeGenerator cg, Type type) { EmitConvertTo(cg, type, false); }
  internal static void EmitConvertTo(CodeGenerator cg, Type type, bool useIndirect)
  { if(type==typeof(void)) throw new ArgumentException("Can't convert to void!");

    if(type.IsValueType || type.IsSubclassOf(typeof(Delegate))) // TODO: be sure to handle all special object types
    { cg.EmitTypeOf(type);
      cg.EmitCall(typeof(Ops), "ConvertTo", typeof(object), typeof(Type));
    }

    if(Options.Current.Debug && type!=typeof(object))
    { Slot tmp = cg.AllocLocalTemp(typeof(object));
      Label good = cg.ILG.DefineLabel();

      cg.Dup();
      tmp.EmitSet(cg);
      cg.ILG.Emit(OpCodes.Isinst, type);
      cg.ILG.Emit(OpCodes.Brtrue_S, good);
      if(!type.IsValueType)
      { tmp.EmitGet(cg);
        cg.ILG.Emit(OpCodes.Brfalse_S, good); // null reference types are okay
      }
      cg.EmitString("expected argument of type "+Ops.TypeName(type)+", but received ");
      tmp.EmitGet(cg);
      cg.EmitCall(typeof(Ops), "TypeName", typeof(object));
      cg.EmitCall(typeof(string), "Concat", typeof(string), typeof(string));
      cg.EmitNew(typeof(ArgumentException), typeof(string));
      cg.ILG.Emit(OpCodes.Throw);
      cg.ILG.MarkLabel(good);
      tmp.EmitGet(cg);

      cg.FreeLocalTemp(tmp);
    }

    if(type.IsValueType)
    { cg.ILG.Emit(OpCodes.Unbox, type);
      if(!useIndirect) cg.EmitIndirectLoad(type);
    }
    else if(type!=typeof(object)) cg.ILG.Emit(OpCodes.Castclass, type);
  }

  internal static void LoadStandardAssemblies()
  { if(!loadedStandard)
    { loadedStandard = true;
      LoadAssemblyByName("mscorlib");
      LoadAssemblyByName("System");
    }
  }

  delegate object Create(IProcedure proc);

  static readonly Hashtable handlers=new Hashtable(), dsigs=new Hashtable();
  static readonly Index dwi=new Index();
  static bool loadedStandard;
}
#endregion

#region ReflectedConstructor
public sealed class ReflectedConstructor : SimpleProcedure, IProperty
{ public ReflectedConstructor(Type type, IProcedure proc)
    : base("#<constructor '"+type.FullName+"'>", 0, -1) { this.proc = proc; }

  #region IProperty Members
  public bool Call(object instance, out object ret, params object[] args)
  { ret = proc.Call(args);
    return true;
  }

  public bool Call(object instance, out object ret, object[] positional, string[] names, object[] values)
  { throw new NotImplementedException("fancy calling .NET methods is not implemented");
  }

  public bool Get(object instance, out object ret) { ret=proc; return true; }
  public bool GetAccessor(object instance, out IProcedure ret) { ret=proc; return true; }
  public bool TrySet(object instance, object value) { return false; }
  #endregion

  public override object Call(object[] args) { return proc.Call(args); }

  readonly IProcedure proc;
}
#endregion

#region ReflectedConstructors
public sealed class ReflectedConstructors : SimpleProcedure, IProperty
{ public ReflectedConstructors(Type type) : base("#<constructor '"+type.FullName+"'>", 0, -1) { this.type = type; }

  #region IProperty Members
  public bool Call(object instance, out object ret, params object[] args)
  { ret = CallCore(args);
    return true;
  }

  public bool Call(object instance, out object ret, object[] positional, string[] names, object[] values)
  { throw new NotImplementedException("fancy calling .NET methods is not implemented");
  }

  public bool Get(object instance, out object ret) { ret=this; return true; }
  public bool GetAccessor(object instance, out IProcedure ret) { ret=this; return true; }
  public bool TrySet(object instance, object value) { return false; }
  #endregion

  public override object Call(object[] args) { return CallCore(args); }

  object CallCore(object[] args)
  { if(funcs==null)
      lock(this)
      { funcs = ReflectedType.GetConstructors(type);
        type  = null;
      }
    return Interop.Call(funcs, args);
  }

  FunctionWrapper[] funcs;
  Type type;
}
#endregion

#region ReflectedEvent
public sealed class ReflectedEvent : IProcedure, IProperty
{ public ReflectedEvent(IProcedure add, IProcedure rem, IProcedure raise, string name, bool isStatic)
  { this.add=add; this.rem=rem; this.raise=raise; this.name=name; this.isStatic=isStatic;
  }
  public ReflectedEvent(ReflectedEvent re, object instance) // assumed to be non-static
  { add=re.add; rem=re.rem; raise=re.raise; name=re.name; this.instance=instance;
  }

  public int MinArgs { get { return raise==null ? 0 : raise.MinArgs; } }
  public int MaxArgs { get { return raise==null ? -1 : raise.MaxArgs; } }
  public bool NeedsFreshArgs { get { return raise==null ? false : raise.NeedsFreshArgs; } }

  public void Add(object func)
  { add.Call(isStatic ? new object[] { func } : new object[] { instance, func });
  }

  public void Remove(object func)
  { rem.Call(isStatic ? new object[] { func } : new object[] { instance, func });
  }

  public object Call(object[] args)
  { if(raise==null) throw new NotSupportedException("this event cannot be raised directly");
    return raise.Call(args);
  }

  #region IProperty Members
  public bool Call(object instance, out object ret, params object[] args)
  { if(raise==null) { ret=null; return false; }
    else { ret=raise.Call(args); return true; }
  }

  public bool Call(object instance, out object ret, object[] positional, string[] names, object[] values)
  { throw new NotImplementedException("fancy calling .NET methods is not implemented");
  }

  public bool Get(object instance, out object ret)
  { ret = isStatic || instance==null ? this : new ReflectedEvent(this, instance);
    return true;
  }

  public bool GetAccessor(object instance, out IProcedure ret)
  { ret = new MemberContainer.SlotAccessor(name);
    return true;
  }

  public bool TrySet(object instance, object value) { return false; }
  #endregion

  public override string ToString() { return "#<event '"+name+"'>"; }

  readonly IProcedure add, rem, raise;
  readonly object instance;
  readonly string name;
  readonly bool isStatic;
}
#endregion

#region ReflectedField
public sealed class ReflectedField : SimpleProcedure, IProperty
{ public ReflectedField(IProcedure proc, string name, bool isStatic, bool readOnly)
    : base("#<field '"+name+"'>", isStatic ? 0 : 1, isStatic ? 1 : 2)
  { this.proc=proc; this.isStatic=isStatic; this.readOnly=readOnly;
  }

  public override object Call(object[] args) { return proc.Call(args); }

  #region IProperty Members
  public bool Call(object instance, out object ret, params object[] args) { ret=proc.Call(args); return true; }

  public bool Call(object instance, out object ret, object[] positional, string[] names, object[] values)
  { throw new NotImplementedException("fancy calling .NET methods is not implemented");
  }

  public bool Get(object instance, out object ret)
  { ret = proc.Call(isStatic ? Ops.EmptyArray : new object[] { instance });
    return true;
  }

  public bool GetAccessor(object instance, out IProcedure ret) { ret=proc; return true; }

  public bool TrySet(object instance, object value)
  { if(readOnly) return false;
    proc.Call(isStatic ? new object[] { value } : new object[] { instance, value });
    return true;
  }
  #endregion

  readonly IProcedure proc;
  readonly bool isStatic, readOnly;
}
#endregion

#region ReflectedFunctions
public sealed class ReflectedFunctions : SimpleProcedure, IProperty
{ // TODO: calculate a rough min/max from 'mis'
  public ReflectedFunctions(MethodInfo[] mis, string name) : base("#<method '"+name+"'>", 0, -1)
  { methods  = mis;
    isStatic = true;
    foreach(MethodInfo mi in mis) if(!mi.IsStatic) { isStatic=false; break; }
  }

  #region IProperty Members
  public bool Call(object instance, out object ret, params object[] args)
  { ret = CallCore(args);
    return true;
  }

  public bool Call(object instance, out object ret, object[] positional, string[] names, object[] values)
  { throw new NotImplementedException("fancy calling .NET methods is not implemented");
  }

  public bool Get(object instance, out object ret)
  { ret = isStatic || instance==null ? (IProcedure)this : new InstanceWrapper(instance, this);
    return true;
  }

  public bool GetAccessor(object instance, out IProcedure ret) { ret=this; return true; }
  public bool TrySet(object instance, object value) { return false; }
  #endregion

  public override object Call(object[] args) { return CallCore(args); }

  object CallCore(object[] args)
  { if(funcs==null)
      lock(this)
      { funcs = new FunctionWrapper[methods.Length];
        for(int i=0; i<methods.Length; i++) funcs[i] = ReflectedType.MakeFunctionWrapper(methods[i]);
        methods = null;
      }
    return Interop.Call(funcs, args);
  }

  FunctionWrapper[] funcs;
  MethodInfo[] methods;
  readonly bool isStatic;
}
#endregion

#region ReflectedNamespace
public sealed class ReflectedNamespace : MemberContainer
{ ReflectedNamespace(string name) { this.name=name; dict=new Hashtable(); }

  public void AddType(Type type)
  { Debug.Assert(type!=null && type.Namespace==name);
    object[] attrs = type.GetCustomAttributes(typeof(ScriptNameAttribute), true);
    dict[attrs.Length==0 ? type.Name : ((ScriptNameAttribute)attrs[0]).Name] = ReflectedType.FromType(type);
  }

  public override bool GetSlot(object instance, string name, out object ret)
  { ret = dict[name];
    return ret!=null;
  }

  public override ICollection GetMemberNames(bool includeImports) { return dict.Keys; }

  public override void Import(TopLevel top, string[] names, string[] asNames)
  { Importer.Import(top, dict, null, names, asNames, "namespace '"+name+"'");
  }

  public override bool TryDeleteSlot(object instance, string name) { return false; }
  public override bool TrySetSlot(object instance, string name, object value) { return false; }

  public override string ToString() { return "#<namespace '"+name+"'>"; }

  public static ReflectedNamespace FromName(string name) { return FromName(name, false); }
  public static ReflectedNamespace FromName(string name, bool returnTop)
  { return FromName(name.Split('.'), returnTop);
  }
  public static ReflectedNamespace FromName(string[] bits, bool returnTop)
  { Interop.LoadStandardAssemblies();

    ReflectedNamespace rns, top;
    lock(cache) top = (ReflectedNamespace)cache[bits[0]];

    if(top==null)
    { top = new ReflectedNamespace(bits[0]);
      lock(cache) cache[bits[0]] = top;
    }
    rns = top;

    string ns = bits[0];
    for(int i=1; i<bits.Length; i++)
    { ns = ns+"."+bits[i];
      object member = rns.dict[bits[i]];
      ReflectedNamespace sub = member as ReflectedNamespace;
      if(sub==null)
      { if(member!=null) throw new ArgumentException("Attempted to load namespace "+string.Join(".", bits)+
                                                     ", but "+ns+" is already used to mean something else");
        rns.dict[bits[i]] = sub = new ReflectedNamespace(ns);
      }
      rns = sub;
    }

    return returnTop ? top : rns;
  }

  string name;
  Hashtable dict;

  static SortedList cache = new SortedList();
}
#endregion

#region ReflectedProperty
public sealed class ReflectedProperty : SimpleProcedure, IProperty
{ public ReflectedProperty(IProcedure get, IProcedure set, string name, int numParams, bool isStatic)
    : base("#<property '"+name+"'>", numParams+(isStatic ? 0 : 1), numParams+(isStatic ? 1 : 2))
  { this.get=get; this.set=set; this.isStatic=isStatic;
  }

  public override object Call(params object[] args)
  { IProcedure proc = args.Length>min ? set : get;
    if(proc==null) throw new InvalidOperationException("this property cannot be "+(args.Length>min ? "set" : "read"));
    return proc.Call(args);
  }

  #region IProperty Members
  public bool Call(object instance, out object ret, params object[] args)
  { IProcedure proc = args.Length>min ? set : get;
    if(proc==null) { ret=null; return false; }
    else { ret=proc.Call(args); return true; }
  }

  public bool Call(object instance, out object ret, object[] positional, string[] names, object[] values)
  { throw new NotImplementedException("fancy calling .NET methods is not implemented");
  }

  public bool Get(object instance, out object ret)
  { if(get==null || min!=(isStatic ? 0 : 1)) { ret=null; return false; }
    ret = get.Call(isStatic ? Ops.EmptyArray : new object[] { instance });
    return true;
  }

  public bool GetAccessor(object instance, out IProcedure ret) { ret=this; return true; }

  public bool TrySet(object instance, object value)
  { if(set==null || min!=(isStatic ? 0 : 1)) return false;
    set.Call(isStatic ? new object[] { value } : new object[] { instance, value });
    return true;
  }
  #endregion

  readonly IProcedure get, set;
  readonly bool isStatic;
}
#endregion

#region ReflectedType
public sealed class ReflectedType : MemberContainer, IProcedure
{ internal ReflectedType(Type type) { Type=type; includeInherited=true; }
  internal ReflectedType(Type type, bool includeInherited) { Type=type; this.includeInherited=includeInherited; }

  #region Static constructor
  static ReflectedType()
  { opnames["op_Addition"]        = "op+";
    opnames["op_BitwiseAnd"]      = "op_bitand";
    opnames["op_BitwiseOr"]       = "op_bitor";
    opnames["op_Decrement"]       = "op--";
    opnames["op_Division"]        = "op/";
    opnames["op_Equality"]        = "op==";
    opnames["op_ExclusiveOr"]     = "op_bitxor";
    opnames["op_GreaterThan"]     = "op>";
    opnames["op_GreaterThanOrEqual"] = "op>=";
    opnames["op_Inequality"]      = "op!=";
    opnames["op_Increment"]       = "op++";
    opnames["op_LeftShift"]       = "op_lshift";
    opnames["op_LessThan"]        = "op<";
    opnames["op_LessThanOrEqual"] = "op<=";
    opnames["op_Modulus"]         = "op%";
    opnames["op_Multiply"]        = "op*";
    opnames["op_OnesComplement"]  = "op_bitnot";
    opnames["op_RightShift"]      = "op_rshift";
    opnames["op_Subtraction"]     = "op-";
    opnames["op_UnaryNegation"]   = "op_neg";
    opnames["op_UnaryPlus"]       = "op_unary-plus";
  }
  #endregion

  public int MinArgs { get { return 0; } }
  public int MaxArgs { get { return -1; } }
  public bool NeedsFreshArgs { get { return false; } }

  public SortedList Dict
  { get
    { if(dict==null) Initialize();
      return dict;
    }
  }

  public object Call(object[] args)
  { if(Constructor==null)
    { if(dict==null) Initialize();
      if(Constructor==null) throw new InvalidOperationException(Ops.TypeName(Type)+" has no available constructors");
    }
    return Constructor.Call(args);
  }

  public override bool GetSlot(object instance, string name, out object ret)
  { if(dict==null) Initialize();
    ret = dict[name];
    return ret!=null || dict.Contains(name);
  }

  public override ICollection GetMemberNames(bool includeImports)
  { if(dict==null) Initialize();
    return dict.Keys;
  }

  public void Import(TopLevel top, bool impersonateLocal) { Import(top, null, null, impersonateLocal); }
  public override void Import(TopLevel top, string[] names, string[] asNames) { Import(top, names, asNames, false); }
  public void Import(TopLevel top, string[] names, string[] asNames, bool impersonateLocal)
  { Importer.Import(top, Dict, impersonateLocal ? top : null, names, asNames, "type '"+Ops.TypeName(Type)+"'");
  }

  public bool IsInstance(object obj)
  { return obj==null ? Type==null : Type==null ? false : Type.IsAssignableFrom(obj.GetType());
  }

  public bool IsSubtype(ReflectedType type)
  { Type ot = type.Type;
    return ot==null ? Type==null : Type==null ? false : Type.IsAssignableFrom(ot);
  }

  public override bool TryDeleteSlot(object instance, string name) { return false; }
  public override bool TrySetSlot(object instance, string name, object value) { return false; }

  public override string ToString() { return "#<type '"+Ops.TypeName(Type)+"'>"; }

  public readonly Type Type;
  public IProcedure Constructor;

  public static ReflectedType FromType(Type type)
  { ReflectedType rt = (ReflectedType)types[type];
    if(rt==null) types[type] = rt = new ReflectedType(type);
    return rt;
  }

  public static ReflectedType FromType(Type type, bool includeInherited)
  { ReflectedType rt = (ReflectedType)types[type];
    if(rt==null) types[type] = rt = new ReflectedType(type, includeInherited);
    return rt;
  }

  public static readonly ReflectedType NullType = new ReflectedType(null);

  sealed class Methods : IDisposable
  { public Methods(Type type) { List=CachedArray.Alloc(); Type=type; }
    public CachedArray List;
    public Type Type;

    public void Dispose() { List.Dispose(); }
  }

  sealed class Properties : IDisposable
  { public Properties(Type type, int numParams)
    { Gets=CachedArray.Alloc(); Sets=CachedArray.Alloc(); Type=type; NumParams=numParams;
    }

    public readonly CachedArray Gets, Sets;
    public Type Type;
    public readonly int NumParams;
    
    public void Dispose() { Gets.Dispose(); Sets.Dispose(); }
  }

  #region Initialize
  void Initialize()
  { Hashtable dict = new Hashtable();
    if(Type==null) return;

    BindingFlags flags = BindingFlags.Public|BindingFlags.Instance|BindingFlags.Static;
    if(!includeInherited) flags |= BindingFlags.DeclaredOnly;

    // TODO: handle certain types specially. eg, delegates, arrays, enums, ...?
    // TODO: don't do any compilation until necessary
    // TODO: speed this up!

    if(!Type.IsPrimitive) // add constructors
    { ConstructorInfo[] ci = Type.GetConstructors();
      bool needDefault = Type.IsValueType && !Type.IsPrimitive; // TODO: should we support primitive constructors?
      if(ci.Length!=0 || needDefault)
        dict[Type.Name] = Constructor =
          ci.Length+(needDefault ? 1 : 0) == 1
            ? new ReflectedConstructor(Type, needDefault ? MakeStructCreator(Type) : MakeFunctionWrapper(ci[0]))
            : (IProcedure)new ReflectedConstructors(Type);
    }

    // add events
    foreach(EventInfo ei in Type.GetEvents(flags))
    { string name = GetMemberName(ei);
      if(ei.DeclaringType!=Type) dict[name] = FromType(ei.DeclaringType).Dict[name];
      else
      { MethodInfo mi = ei.GetAddMethod();
        bool isStatic = mi.IsStatic;
        IProcedure add=MakeFunctionWrapper(mi), rem=MakeFunctionWrapper(ei.GetRemoveMethod());
        dict[name] = new ReflectedEvent(add, rem, mi==null ? null : MakeFunctionWrapper(mi), name, isStatic);
      }
    }

    // add fields
    foreach(FieldInfo fi in Type.GetFields(flags))
    { string name = GetMemberName(fi);
      dict[name] = fi.DeclaringType==Type
        ? new ReflectedField(MakeFieldWrapper(fi), name, fi.IsStatic, fi.IsInitOnly || fi.IsLiteral)
        : FromType(fi.DeclaringType).Dict[name];
    }

    // add properties
    ListDictionary overloads = new ListDictionary();
    foreach(PropertyInfo pi in Type.GetProperties(flags))
    { string name = GetMemberName(pi);
      MethodInfo get = pi.CanRead ? pi.GetGetMethod() : null;
      MethodInfo set = pi.CanWrite ? pi.GetSetMethod() : null;

      Properties ps = (Properties)overloads[name];
      Type declaringType = MostDerived(get, set);
      if(ps==null) overloads[name] = ps = new Properties(declaringType, pi.GetIndexParameters().Length);
      else if(declaringType.IsSubclassOf(ps.Type)) ps.Type = declaringType;
      if(get!=null) ps.Gets.Add(get);
      if(set!=null) ps.Sets.Add(set);
    }

    foreach(DictionaryEntry de in overloads)
    { string name = (string)de.Key;
      Properties ps = (Properties)de.Value;
      if(ps.Type!=Type) dict[name] = FromType(ps.Type).Dict[name];
      else
      { IProcedure get = ps.Gets.Count==0 ? null
                           : ps.Gets.Count==1 ? (IProcedure)MakeFunctionWrapper((MethodInfo)ps.Gets[0])
                           : new ReflectedFunctions((MethodInfo[])ps.Gets.ToArray(typeof(MethodInfo)), "get_"+name);
        IProcedure set = ps.Sets.Count==0 ? null
                           : ps.Sets.Count==1 ? (IProcedure)MakeFunctionWrapper((MethodInfo)ps.Sets[0])
                           : new ReflectedFunctions((MethodInfo[])ps.Sets.ToArray(typeof(MethodInfo)), "set_"+name);
        bool isStatic = ((MethodInfo)(ps.Gets.Count==0 ? ps.Sets : ps.Gets)[0]).IsStatic;
        dict[name] = new ReflectedProperty(get, set, name, ps.NumParams, isStatic);
      }
      ps.Dispose();
    }

    // add methods
    overloads.Clear();
    foreach(MethodInfo mi in Type.GetMethods(flags))
      if(!IsSpecialMethod(mi))
      { string name=GetMemberName(mi), op=(string)opnames[name];
        if(op!=null) name = op;
        Methods ov = (Methods)overloads[name];
        if(ov==null) overloads[name] = ov = new Methods(mi.DeclaringType);
        else if(mi.DeclaringType.IsSubclassOf(ov.Type)) ov.Type = mi.DeclaringType;
        ov.List.Add(mi);
      }
    foreach(DictionaryEntry de in overloads)
    { Methods ov = (Methods)de.Value;
      dict[de.Key] = ov.Type!=Type    ? FromType(ov.Type).Dict[de.Key] :
                     ov.List.Count==1 ? (IProcedure)MakeFunctionWrapper((MethodInfo)ov.List[0])
                       : new ReflectedFunctions((MethodInfo[])ov.List.ToArray(typeof(MethodInfo)), (string)de.Key);
      ov.Dispose();
    }

    flags = BindingFlags.Public | (includeInherited ? 0 : BindingFlags.DeclaredOnly);
    foreach(Type subtype in Type.GetNestedTypes(flags))
      if(subtype.IsSubclassOf(typeof(Primitive)))
      { Primitive prim = (Primitive)subtype.GetConstructor(Type.EmptyTypes).Invoke(null);
        dict[prim.Name] = prim;
      }
      else dict[GetMemberName(subtype)] = ReflectedType.FromType(subtype);

    this.dict = new SortedList(dict);
  }
  #endregion

  SortedList dict;
  bool includeInherited;

  internal static FunctionWrapper[] GetConstructors(Type type)
  { ConstructorInfo[] ci = type.GetConstructors();
    bool needDefault = type.IsValueType && !type.IsPrimitive;
    FunctionWrapper[] ret = new FunctionWrapper[ci.Length + (needDefault ? 1 : 0)];
    for(int i=0; i<ci.Length; i++) ret[i] = MakeFunctionWrapper(ci[i]);
    if(needDefault) ret[ci.Length] = MakeStructCreator(type);
    return ret;
  }

  static string GetMemberName(MemberInfo mi)
  { object[] attrs = mi.GetCustomAttributes(typeof(ScriptNameAttribute), false);
    return attrs.Length==0 ? mi.Name : ((ScriptNameAttribute)attrs[0]).Name;
  }

  static bool IsSpecialMethod(MethodInfo mi)
  { return mi.IsSpecialName && (mi.Name.StartsWith("get_") || mi.Name.StartsWith("set_") ||
                                mi.Name!="op_Implicit" || mi.Name!="op_Explicit"); // TODO: handle these ops somehow?
  }

  static Type MostDerived(MethodInfo f1, MethodInfo f2)
  { if(f1==null) return f2.DeclaringType;
    else if(f2==null || f1.DeclaringType.IsSubclassOf(f2.DeclaringType)) return f1.DeclaringType;
    else return f2.DeclaringType;
  }

  #region MakeFieldWrapper
  static FieldWrapper MakeFieldWrapper(FieldInfo fi)
  { TypeGenerator tg = SnippetMaker.Assembly.DefineType(TypeAttributes.Public|TypeAttributes.Sealed,
                                                        "fw"+fwi.Next+"$"+fi.Name, typeof(FieldWrapper));
    CodeGenerator cg = tg.DefineMethodOverride("Call", true);
    Label set;
    bool readOnly = fi.IsInitOnly || fi.IsLiteral;

    if(readOnly) set = cg.ILG.DefineLabel();
    else
    { set = cg.ILG.DefineLabel();
      cg.EmitArgGet(0);
      cg.ILG.Emit(OpCodes.Ldlen);
      cg.EmitInt(fi.IsStatic ? 1 : 2);
      cg.ILG.Emit(OpCodes.Beq_S, set);
    }

    if(!fi.IsStatic)
    { if(Options.Current.Debug)
      { Label good = cg.ILG.DefineLabel();
        cg.EmitArgGet(0);
        cg.ILG.Emit(OpCodes.Ldlen);
        cg.EmitInt(1);
        cg.ILG.Emit(OpCodes.Bge_S, good);
        cg.EmitString("non-static field getter expects 1 argument");
        cg.EmitNew(typeof(ArgumentException), typeof(string));
        cg.ILG.Emit(OpCodes.Throw);
        cg.ILG.MarkLabel(good);
      }
      cg.EmitArgGet(0);
      cg.EmitInt(0);
      cg.ILG.Emit(OpCodes.Ldelem_Ref);
      if(fi.DeclaringType != typeof(object))
        cg.ILG.Emit(fi.DeclaringType.IsValueType ? OpCodes.Unbox : OpCodes.Castclass, fi.DeclaringType);
    }
    cg.EmitFieldGet(fi.DeclaringType, fi.Name);
    if(fi.FieldType.IsValueType) cg.ILG.Emit(OpCodes.Box, fi.FieldType);
    cg.EmitReturn();

    if(!readOnly)
    { cg.ILG.MarkLabel(set);
      if(!fi.IsStatic)
      { cg.EmitArgGet(0);
        cg.EmitInt(0);
        cg.ILG.Emit(OpCodes.Ldelem_Ref);
        if(fi.DeclaringType != typeof(object))
          cg.ILG.Emit(fi.DeclaringType.IsValueType ? OpCodes.Unbox : OpCodes.Castclass, fi.DeclaringType);
      }

      cg.EmitArgGet(0);
      cg.EmitInt(fi.IsStatic ? 0 : 1);
      cg.ILG.Emit(OpCodes.Ldelem_Ref);
      if(UsesIndirectCopy(fi.FieldType))
      { cg.EmitFieldGetAddr(fi.DeclaringType, fi.Name);
        Interop.EmitConvertTo(cg, fi.FieldType, true);
        cg.ILG.Emit(OpCodes.Cpobj, fi.FieldType);
      }
      else
      { Interop.EmitConvertTo(cg, fi.FieldType);
        cg.EmitFieldSet(fi.DeclaringType, fi.Name);
      }
      cg.EmitNull();
      cg.EmitReturn();
    }
    cg.Finish();

    cg = tg.DefineConstructor(Type.EmptyTypes);
    cg.EmitThis();
    cg.EmitString(fi.IsStatic ? fi.DeclaringType.FullName+"."+fi.Name : fi.Name);
    cg.EmitBool(fi.IsStatic);
    cg.EmitCall(typeof(FieldWrapper).GetConstructor(new Type[] { typeof(string), typeof(bool) }));
    cg.EmitReturn();
    cg.Finish();

    return (FieldWrapper)tg.FinishType().GetConstructor(Type.EmptyTypes).Invoke(null);
  }
  #endregion
  
  #region MakeFunctionWrapper
  internal static FunctionWrapper MakeFunctionWrapper(MethodBase mi)
  { bool isCons = mi is ConstructorInfo;
    return (FunctionWrapper)MakeSignatureWrapper(mi)
            .GetConstructor(isCons ? Type.EmptyTypes : new Type[] { typeof(IntPtr), typeof(bool) })
            .Invoke(isCons ? Ops.EmptyArray : new object[] { mi.MethodHandle.GetFunctionPointer(), mi.IsStatic });
  }
  #endregion

  #region MakeSignatureWrapper
  struct Ref
  { public Ref(int i, Slot slot) { Index=i; Slot=slot; }
    public Slot Slot;
    public int Index;
  }

  static Type MakeSignatureWrapper(MethodBase mi)
  { Signature sig = new Signature(mi);
    Type type = (Type)sigs[sig];

    if(type==null)
    { bool isCons = mi is ConstructorInfo;

      string name = "sw$"+swi.Next.ToString();
      #if DEBUG
      for(int i=0; i<sig.Params.Length; i++)
      { Type t = sig.Params[i];
        string suf=null;
        while(true)
        { if(t.IsPointer) suf += "Ptr";
          else if(t.IsArray) suf += "Arr";
          else if(t.GetElementType()==null) break;
          t = t.GetElementType();
        }
        name += "_"+t.Name+suf;
      }
      #endif
      TypeGenerator tg = SnippetMaker.Assembly.DefineType(TypeAttributes.Public|TypeAttributes.Sealed,
                                                          name, isCons ? typeof(FunctionWrapper)
                                                                       : typeof(FunctionWrapperI));
      int numnp = sig.ParamArray ? sig.Params.Length-1 : sig.Params.Length;
      int min = sig.Params.Length - (sig.Defaults==null ? 0 : sig.Defaults.Length) - (sig.ParamArray ? 1 : 0);
      int max = sig.ParamArray ? -1 : sig.Params.Length;
      int refi=0, numrefs=0;
      for(int i=0; i<sig.Params.Length; i++) if(sig.Params[i].IsByRef) numrefs++;
      Ref[] refs = new Ref[numrefs];

      #region Initialize statics
      Slot ptypes =
        sig.Params.Length==0 ? null : tg.DefineStaticField(FieldAttributes.Private, "ptypes", typeof(Type[]));
      Slot defaults =
        sig.Defaults==null ? null : tg.DefineStaticField(FieldAttributes.Private, "defaults", typeof(object[]));

      CodeGenerator cg = tg.GetInitializer();
      if(ptypes!=null) // ptypes
      { cg.EmitNewArray(typeof(Type), sig.Params.Length);
        for(int i=0; i<sig.Params.Length; i++)
        { cg.Dup();
          cg.EmitInt(i);
          cg.EmitTypeOf(sig.Params[i]);
          cg.ILG.Emit(OpCodes.Stelem_Ref);
        }
        ptypes.EmitSet(cg);
      }

      if(defaults!=null) // defaults
      { cg.EmitObjectArray(sig.Defaults);
        defaults.EmitSet(cg);
      }
      #endregion

      #region Constructor
      if(!isCons)
      { cg = tg.DefineChainedConstructor(typeof(IntPtr), typeof(bool));
        cg.EmitReturn();
        cg.Finish();
      }
      #endregion

      #region MinArgs and MaxArgs
      cg = tg.DefinePropertyOverride("MinArgs", true);
      cg.EmitInt(min);
      cg.EmitReturn();
      cg.Finish();

      cg = tg.DefinePropertyOverride("MaxArgs", true);
      cg.EmitInt(max);
      cg.EmitReturn();
      cg.Finish();
      #endregion

      #region TryMatch
      cg = tg.DefineMethodOverride(tg.BaseType.GetMethod("TryMatch", BindingFlags.Public|BindingFlags.Instance), true);
      cg.EmitArgGet(0);
      cg.EmitArgGet(1);
      if(ptypes!=null) ptypes.EmitGet(cg);
      else cg.EmitFieldGet(typeof(Type), "EmptyTypes");
      cg.EmitInt(numnp);
      cg.EmitInt(min);
      cg.EmitBool(sig.ParamArray);
      cg.EmitCall(tg.BaseType.GetMethod("TryMatch", BindingFlags.Static|BindingFlags.NonPublic|BindingFlags.FlattenHierarchy));
      cg.EmitReturn();
      cg.Finish();
      #endregion

      MethodInfo checkArity = null;
      #region CheckArity
      if(!sig.ParamArray || min!=0)
      { // CheckArity
        cg = tg.DefineStaticMethod(MethodAttributes.Private, "CheckArity",
                                   typeof(void), new Type[] { typeof(object[]) });
        checkArity = (MethodInfo)cg.MethodBase;
        Label bad = cg.ILG.DefineLabel();
        Slot  len = cg.AllocLocalTemp(typeof(int));
        cg.EmitArgGet(0);
        cg.ILG.Emit(OpCodes.Ldlen);
        if(sig.ParamArray)
        { cg.EmitInt(min);
          cg.ILG.Emit(OpCodes.Blt_S, bad);
          cg.EmitReturn();
          cg.ILG.MarkLabel(bad);
          cg.EmitString((isCons ? "constructor" : "function")+" expects at least "+min.ToString()+
                        " arguments, but received ");
          cg.EmitArgGet(0);
          cg.ILG.Emit(OpCodes.Ldlen);
          len.EmitSet(cg);
          len.EmitGetAddr(cg);
          cg.EmitCall(typeof(int), "ToString", Type.EmptyTypes);
          cg.EmitCall(typeof(String), "Concat", typeof(string), typeof(string));
          cg.EmitNew(typeof(ArgumentException), typeof(string));
          cg.ILG.Emit(OpCodes.Throw);
        }
        else
        { cg.Dup();
          len.EmitSet(cg);
          cg.EmitInt(min);
          if(min==max) cg.ILG.Emit(OpCodes.Bne_Un_S, bad);
          else
          { cg.ILG.Emit(OpCodes.Blt_S, bad);
            len.EmitGet(cg);
            cg.EmitInt(max);
            cg.ILG.Emit(OpCodes.Bgt_S, bad);
          }
          cg.EmitReturn();
          cg.ILG.MarkLabel(bad);
          cg.EmitString((isCons ? "constructor" : "function")+" expects "+min.ToString()+
                        (min==max ? "" : "-"+max.ToString())+" arguments, but received ");
          len.EmitGetAddr(cg);
          cg.EmitCall(typeof(int), "ToString", Type.EmptyTypes);
          cg.EmitCall(typeof(String), "Concat", typeof(string), typeof(string));
          cg.EmitNew(typeof(ArgumentException), typeof(string));
          cg.ILG.Emit(OpCodes.Throw);
        }
        cg.FreeLocalTemp(len);
        cg.Finish();
      }
      #endregion

      #region Call
      cg = tg.DefineMethodOverride("Call", true, typeof(object[]));

      if(checkArity!=null)
      { cg.EmitArgGet(0);
        cg.EmitCall(checkArity);
      }

      for(int i=0; i<min; i++) // required arguments
      { cg.EmitArgGet(0);
        cg.EmitInt(i);
        cg.ILG.Emit(OpCodes.Ldelem_Ref);
        if(sig.Params[i].IsByRef)
        { Type etype = sig.Params[i].GetElementType();
          Interop.EmitConvertTo(cg, typeof(Reference));
          cg.EmitFieldGet(typeof(Reference), "Value");
          Interop.EmitConvertTo(cg, etype);
          Slot tmp = cg.AllocLocalTemp(etype);
          tmp.EmitSet(cg);
          tmp.EmitGetAddr(cg);
          refs[refi++] = new Ref(i, tmp);
        }
        else if(sig.Params[i].IsPointer) Interop.EmitConvertTo(cg, typeof(IntPtr));
        else Interop.EmitConvertTo(cg, sig.Params[i], i==0 && sig.PointerHack!=IntPtr.Zero);
      }

      if(min<numnp) // default arguments
      { Slot len = cg.AllocLocalTemp(typeof(int)); // TODO: test this code
        cg.EmitArgGet(0);
        cg.ILG.Emit(OpCodes.Ldlen);
        len.EmitSet(cg);

        for(int i=min; i<numnp; i++)
        { Label next=cg.ILG.DefineLabel(), useArg=cg.ILG.DefineLabel();
          len.EmitGet(cg);
          cg.EmitInt(i);
          cg.ILG.Emit(OpCodes.Bgt_Un_S, useArg);
          cg.EmitConstant(sig.Defaults[i]);
          cg.ILG.Emit(OpCodes.Br_S, next);
          cg.ILG.MarkLabel(useArg);
          cg.EmitArgGet(0);
          cg.EmitInt(i);
          cg.ILG.Emit(OpCodes.Ldelem_Ref);
          Interop.EmitConvertTo(cg, sig.Params[i]);
          cg.ILG.MarkLabel(next);
        }

        cg.FreeLocalTemp(len);
      }

      #region ParamArray handling
      if(sig.ParamArray)
      { Type etype = sig.Params[numnp].GetElementType();
        if(min==0 && etype==typeof(object)) cg.EmitArgGet(0);
        else
        { Slot iv=cg.AllocLocalTemp(typeof(int)), sa=cg.AllocLocalTemp(typeof(Array));
          Label pack=cg.ILG.DefineLabel(), call=cg.ILG.DefineLabel(), loop;
          bool ind = UsesIndirectCopy(etype);

          #region Handle array casting
          cg.EmitArgGet(0); // if(args.Length==ptypes.Length) {
          cg.ILG.Emit(OpCodes.Ldlen);
          cg.EmitInt(sig.Params.Length);
          cg.ILG.Emit(OpCodes.Bne_Un, pack);

          cg.EmitArgGet(0); // sa = args[numNP] as Array;
          cg.EmitInt(numnp);
          cg.ILG.Emit(OpCodes.Ldelem_Ref);
          cg.ILG.Emit(OpCodes.Isinst, typeof(Array));
          cg.Dup();
          sa.EmitSet(cg);
          cg.ILG.Emit(OpCodes.Brfalse, pack); // if(sa==null) goto pack

          sa.EmitGet(cg); // conv = Ops.ConvertTo(sa.GetType(), ptypes[numNP]);
          cg.EmitCall(typeof(Array), "GetType");
          cg.EmitTypeOf(sig.Params[numnp]);
          cg.EmitCall(typeof(Ops), "ConvertTo", typeof(Type), typeof(Type));
          cg.Dup(); // used below
          iv.EmitSet(cg);

          // if(conv==Identity || conv==Reference) { stack.push(castTo(ptypes[numNP], sa)); goto call; }
          Label not2=cg.ILG.DefineLabel(), is2=cg.ILG.DefineLabel();
          cg.EmitInt((int)Conversion.Identity);
          cg.ILG.Emit(OpCodes.Beq_S, is2);
          iv.EmitGet(cg);
          cg.EmitInt((int)Conversion.Reference);
          cg.ILG.Emit(OpCodes.Bne_Un_S, not2);
          cg.ILG.MarkLabel(is2);
          sa.EmitGet(cg);
          cg.ILG.Emit(OpCodes.Castclass, sig.Params[numnp]);
          cg.ILG.Emit(OpCodes.Br, call);
          #endregion

          #region Handle array conversion
          cg.ILG.MarkLabel(not2);
          if(etype!=typeof(object))
          { sa.EmitGet(cg); // conv = Ops.ConvertTo(sa.GetType(), ptypes[numnp].GetElementType());
            cg.EmitCall(typeof(Array), "GetType");
            cg.EmitTypeOf(etype);
            cg.EmitCall(typeof(Ops), "ConvertTo", typeof(Type), typeof(Type));
            cg.Dup(); // used below
            iv.EmitSet(cg);

            // if(conv==Identity || conv==Reference) goto pack;
            cg.EmitInt((int)Conversion.Identity);
            cg.ILG.Emit(Options.Current.Debug ? OpCodes.Beq : OpCodes.Beq_S, pack);
            iv.EmitGet(cg);
            cg.EmitInt((int)Conversion.Reference);
            cg.ILG.Emit(Options.Current.Debug ? OpCodes.Beq : OpCodes.Beq_S, pack);

            sa.EmitGet(cg); // etype[] pa = new etype[sa.Length];
            cg.EmitPropGet(typeof(Array), "Length");
            cg.ILG.Emit(OpCodes.Newarr, etype);

            cg.EmitInt(numnp); // for(int i=0; i<pa.Length; i++) pa[i] = ConvertTo(sa[i], etype);
            iv.EmitSet(cg);
            loop = cg.ILG.DefineLabel();
            cg.ILG.MarkLabel(loop);
            cg.Dup();
            cg.ILG.Emit(OpCodes.Ldlen);
            iv.EmitGet(cg);
            cg.ILG.Emit(Options.Current.Debug ? OpCodes.Beq : OpCodes.Beq_S, call);
            cg.Dup();
            iv.EmitGet(cg);
            if(ind) cg.ILG.Emit(OpCodes.Ldelema);
            sa.EmitGet(cg); // sa[i]
            iv.EmitGet(cg);
            cg.EmitCall(typeof(Array), "GetValue", typeof(int));
            Interop.EmitConvertTo(cg, etype, ind);
            if(ind) cg.ILG.Emit(OpCodes.Cpobj, etype);
            else cg.EmitArrayStore(etype);
            iv.EmitGet(cg); // i++
            cg.EmitInt(1);
            cg.ILG.Emit(OpCodes.Add);
            iv.EmitSet(cg);
            cg.ILG.Emit(OpCodes.Br_S, loop);
          }
          #endregion

          #region Handle new array packing
          cg.ILG.MarkLabel(pack); // pack:
          cg.EmitArgGet(0);       // etype[] pa = new etype[args.Length - numnp];
          cg.ILG.Emit(OpCodes.Ldlen);
          cg.EmitInt(numnp);
          cg.ILG.Emit(OpCodes.Sub);
          if(etype==typeof(object))
          { cg.Dup(); // used below
            iv.EmitSet(cg);
          }
          cg.ILG.Emit(OpCodes.Newarr, etype);

          if(etype==typeof(object)) // Array.Copy(args, numnp, pa, 0, pa.Length)
          { Slot pa=cg.AllocLocalTemp(sig.Params[numnp]);
            pa.EmitSet(cg);
            cg.EmitArgGet(0);
            cg.EmitInt(numnp);
            pa.EmitGet(cg);
            cg.EmitInt(0);
            iv.EmitGet(cg);
            cg.EmitCall(typeof(Array), "Copy",
                        new Type[] { typeof(Array), typeof(int), typeof(Array), typeof(int), typeof(int) });
            pa.EmitGet(cg);
            cg.FreeLocalTemp(pa);
          }
          else
          { cg.EmitInt(numnp); // for(int i=numnp; i<args.Length; i++) pa[i-numnp] = ConvertTo(args[i], etype);
            iv.EmitSet(cg);
            loop = cg.ILG.DefineLabel();
            cg.ILG.MarkLabel(loop);
            iv.EmitGet(cg);
            cg.EmitArgGet(0);
            cg.ILG.Emit(OpCodes.Ldlen);
            cg.ILG.Emit(OpCodes.Beq_S, call);

            cg.Dup(); // dup pa
            iv.EmitGet(cg);
            cg.EmitInt(numnp);
            cg.ILG.Emit(OpCodes.Sub);
            if(ind) cg.ILG.Emit(OpCodes.Ldelema);
            cg.EmitArgGet(0);
            iv.EmitGet(cg);
            cg.ILG.Emit(OpCodes.Ldelem_Ref);
            Interop.EmitConvertTo(cg, etype, ind);
            if(ind) cg.ILG.Emit(OpCodes.Cpobj, etype);
            else cg.EmitArrayStore(etype);
            iv.EmitGet(cg);
            cg.EmitInt(1);
            cg.ILG.Emit(OpCodes.Add);
            iv.EmitSet(cg);
            cg.ILG.Emit(OpCodes.Br_S, loop);
          }
          #endregion

          cg.ILG.MarkLabel(call);
          cg.FreeLocalTemp(iv);
          cg.FreeLocalTemp(sa);
        }
      }
      #endregion

      if(isCons)
      { cg.EmitNew((ConstructorInfo)mi);
        if(mi.DeclaringType.IsValueType) cg.ILG.Emit(OpCodes.Box, mi.DeclaringType);
      }
      else
      { // TODO: report this to microsoft and see if we can get a straight answer
        if(sig.PointerHack!=IntPtr.Zero) cg.ILG.Emit(OpCodes.Ldftn, (MethodInfo)mi); // HACK: we hardcode the function pointer in this case because MethodHandle.GetFunctionPointer() doesn't return the correct value for instance calls on value types. i'm not sure if this is safe, but it seems to work.
        else
        { cg.EmitThis();
          cg.EmitFieldGet(typeof(FunctionWrapperI), "methodPtr");
        }
        if(!sig.Return.IsValueType && numrefs==0) cg.ILG.Emit(OpCodes.Tailcall);
        cg.ILG.EmitCalli(OpCodes.Calli, sig.Convention, sig.Return, sig.Params, null);
        if(sig.Return==typeof(void)) cg.EmitNull();
        else if(sig.Return.IsValueType) cg.ILG.Emit(OpCodes.Box, sig.Return);

        foreach(Ref r in refs)
        { Type etype = sig.Params[r.Index].GetElementType();
          cg.EmitArgGet(0);
          cg.EmitInt(r.Index);
          cg.ILG.Emit(OpCodes.Ldelem_Ref);
          cg.ILG.Emit(OpCodes.Castclass, typeof(Reference));
          r.Slot.EmitGet(cg);
          if(etype.IsValueType) cg.ILG.Emit(OpCodes.Box, etype);
          cg.EmitFieldSet(typeof(Reference), "Value");
          cg.FreeLocalTemp(r.Slot);
        }
      }

      cg.EmitReturn();
      cg.Finish();
      #endregion
      sigs[sig] = type = tg.FinishType();
    }
    return type;
  }
  #endregion

  #region MakeStructCreator
  internal static StructCreator MakeStructCreator(Type type)
  { TypeGenerator tg = SnippetMaker.Assembly.DefineType(TypeAttributes.Public|TypeAttributes.Sealed,
                                                        "sc$"+sci.Next, typeof(StructCreator));
    CodeGenerator cg = tg.DefineMethodOverride("Call", true);
    Slot slot = cg.AllocLocalTemp(type);
    slot.EmitGetAddr(cg);
    cg.ILG.Emit(OpCodes.Initobj, type);
    slot.EmitGet(cg);
    cg.ILG.Emit(OpCodes.Box, type);
    cg.EmitReturn();
    cg.Finish();
    return (StructCreator)tg.FinishType().GetConstructor(Type.EmptyTypes).Invoke(null);
  }
  #endregion

  static bool UsesIndirectCopy(Type type) // TODO: see if it's faster to always use indirect copying
  { if(!type.IsValueType || type.IsPointer || type==typeof(IntPtr)) return false;
    TypeCode code = Type.GetTypeCode(type);
    return code==TypeCode.Object || code==TypeCode.DateTime || code==TypeCode.DBNull || code==TypeCode.Decimal;
  }

  static readonly Hashtable types=new Hashtable(), sigs=new Hashtable();
  static readonly SortedList opnames=new SortedList();
  static readonly Index fwi=new Index(), swi=new Index(), sci=new Index();
}
#endregion

#region Signature
sealed class Signature
{ public Signature(MethodBase mi) : this(mi, false) { }
  public Signature(MethodBase mi, bool ignoreThis)
  { ParameterInfo[] pi = mi.GetParameters();

    IsCons   = mi is ConstructorInfo;
    int so   = IsCons || mi.IsStatic || ignoreThis ? 0 : 1;

    Convention = mi.CallingConvention==CallingConventions.VarArgs ? CallingConventions.VarArgs
                                                                  : CallingConventions.Standard;
    Return     = IsCons ? mi.DeclaringType : ((MethodInfo)mi).ReturnType;
    Params     = new Type[pi.Length + so];
    ParamArray = pi.Length>0 && pi[pi.Length-1].IsDefined(typeof(ParamArrayAttribute), false);

    if(so==1)
    { Params[0] = mi.DeclaringType;
      PointerHack = mi.DeclaringType.IsValueType ? mi.MethodHandle.Value : IntPtr.Zero;
    }
    for(int i=0,req=-1; i<pi.Length; i++)
    { Params[i + so] = pi[i].ParameterType;
      if(pi[i].IsOptional)
      { if(req==-1)
        { req = i;
          Defaults = new object[pi.Length - i - (ParamArray ? 1 : 0)];
        }
        Defaults[i-req] = pi[i].DefaultValue;
      }
    }
  }

  public override bool Equals(object obj)
  { Signature o = (Signature)obj;
    if(Params.Length!=o.Params.Length || ParamArray!=o.ParamArray || Return!=o.Return || Convention!=o.Convention ||
        IsCons!=o.IsCons || PointerHack!=o.PointerHack ||
       (Defaults==null && o.Defaults!=null || Defaults!=null && o.Defaults==null ||
        Defaults!=null && Defaults.Length!=o.Defaults.Length))
      return false;
    for(int i=0; i<Params.Length; i++) if(Params[i] != o.Params[i]) return false;
    if(Defaults!=null)
      for(int i=0; i<Defaults.Length; i++) if(!object.Equals(Defaults[i], o.Defaults[i])) return false;
    return true;
  }

  public override int GetHashCode()
  { int hash=Return.GetHashCode();
    for(int i=0; i<Params.Length; i++) hash ^= Params[i].GetHashCode();
    return hash;
  }

  public Type     Return;
  public Type[]   Params;
  public object[] Defaults;
  public CallingConventions Convention;
  public IntPtr PointerHack;
  public bool     IsCons, ParamArray;
}
#endregion

#region StructCreator
public abstract class StructCreator : FunctionWrapper
{ public override int MaxArgs { get { return 0; } }
  public override int MinArgs { get { return 0; } }
  public override Conversion TryMatch(object[] args, Type[] types)
  { return TryMatch(args, types, Type.EmptyTypes, 0, 0, false);
  }
}
#endregion

} // namespace Scripting