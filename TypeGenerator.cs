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
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;

namespace Scripting.Backend
{

public sealed class TypeGenerator
{ public TypeGenerator(AssemblyGenerator assembly, TypeBuilder typeBuilder)
  { Assembly=assembly; TypeBuilder=typeBuilder;
  }

  [Flags] public enum PropertyOverride { Read=1, Write=2, Either=Read|Write };

  public Type BaseType { get { return TypeBuilder.BaseType; } }
  public bool IsSealed { get { return (TypeBuilder.Attributes&TypeAttributes.Sealed)!=0; } }

  public CodeGenerator DefineConstructor(params Type[] types) { return DefineConstructor(MethodAttributes.Public, types); }
  public CodeGenerator DefineConstructor(MethodAttributes attrs, params Type[] types)
  { return new CodeGenerator(this, TypeBuilder.DefineConstructor(attrs, CallingConventions.Standard, types));
  }

  public CodeGenerator DefineChainedConstructor(params Type[] paramTypes)
  { ConstructorInfo ci = TypeBuilder.BaseType.GetConstructor(BindingFlags.Instance|BindingFlags.Public|BindingFlags.NonPublic,
                                                             null, paramTypes, null);
    if(ci.IsPrivate) ci = null;
    return DefineChainedConstructor(ci);
  }
  public CodeGenerator DefineChainedConstructor(ConstructorInfo parent)
  { ParameterInfo[] pi = parent.GetParameters();
    Type[] types = GetParamTypes(pi);
    ConstructorBuilder cb = TypeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, types);
    for(int i=0; i<pi.Length; i++)
    { ParameterBuilder pb = cb.DefineParameter(i+1, pi[i].Attributes, pi[i].Name);
      if(pi[i].IsDefined(typeof(ParamArrayAttribute), false))
        pb.SetCustomAttribute(
          new CustomAttributeBuilder(typeof(ParamArrayAttribute).GetConstructor(Type.EmptyTypes), Ops.EmptyArray));
    }

    CodeGenerator cg = new CodeGenerator(this, cb);
    cg.EmitThis();
    for(int i=0; i<pi.Length; i++) cg.EmitArgGet(i);
    cg.EmitCall(parent);
    return cg;
  }

  public CodeGenerator DefineDefaultConstructor() { return DefineDefaultConstructor(MethodAttributes.Public); }
  public CodeGenerator DefineDefaultConstructor(MethodAttributes attrs)
  { return new CodeGenerator(this, TypeBuilder.DefineDefaultConstructor(attrs));
  }

  public Slot DefineField(string name, Type type) { return DefineField(FieldAttributes.Public, name, type); }
  public Slot DefineField(FieldAttributes attrs, string name, Type type)
  { return new FieldSlot(new ThisSlot(TypeBuilder), TypeBuilder.DefineField(name, type, attrs));
  }

  public CodeGenerator DefineMethod(string name, Type retType, params Type[] paramTypes)
  { return DefineMethod(MethodAttributes.Public, name, IsSealed, retType, paramTypes);
  }
  public CodeGenerator DefineMethod(string name, bool final, Type retType, params Type[] paramTypes)
  { return DefineMethod(MethodAttributes.Public, name, final, retType, paramTypes);
  }
  public CodeGenerator DefineMethod(MethodAttributes attrs, string name, Type retType, params Type[] paramTypes)
  { return DefineMethod(attrs, name, IsSealed, retType, paramTypes);
  }
  public CodeGenerator DefineMethod(MethodAttributes attrs, string name, bool final,
                                    Type retType, params Type[] paramTypes)
  { if((attrs&MethodAttributes.Static)!=0) attrs &= ~MethodAttributes.Final;
    else if(final) attrs |= MethodAttributes.Final;
    return new CodeGenerator(this, TypeBuilder.DefineMethod(name, attrs, retType, paramTypes));
  }

  public CodeGenerator DefineMethodOverride(string name)
  { return DefineMethodOverride(TypeBuilder.BaseType, name, IsSealed);
  }
  public CodeGenerator DefineMethodOverride(string name, params Type[] paramTypes)
  { return DefineMethodOverride(TypeBuilder.BaseType, name, IsSealed, paramTypes);
  }
  public CodeGenerator DefineMethodOverride(string name, bool final)
  { return DefineMethodOverride(TypeBuilder.BaseType.GetMethod(name, BindingFlags.Instance|BindingFlags.NonPublic|BindingFlags.Public),
                                final);
  }
  public CodeGenerator DefineMethodOverride(string name, bool final, params Type[] paramTypes)
  { return DefineMethodOverride(TypeBuilder.BaseType, name, final, paramTypes);
  }
  public CodeGenerator DefineMethodOverride(Type type, string name)
  { return DefineMethodOverride(type, name, IsSealed);
  }
  public CodeGenerator DefineMethodOverride(Type type, string name, params Type[] paramTypes)
  { return DefineMethodOverride(type, name, IsSealed, paramTypes);
  }
  public CodeGenerator DefineMethodOverride(Type type, string name, bool final)
  { return DefineMethodOverride(type.GetMethod(name, BindingFlags.Instance|BindingFlags.NonPublic|BindingFlags.Public),
                                final);
  }
  public CodeGenerator DefineMethodOverride(Type type, string name, bool final, params Type[] paramTypes)
  { return DefineMethodOverride(type.GetMethod(name, BindingFlags.Instance|BindingFlags.NonPublic|BindingFlags.Public,
                                               null, paramTypes, null), final);
  }
  public CodeGenerator DefineMethodOverride(MethodInfo baseMethod)
  { return DefineMethodOverride(baseMethod, IsSealed);
  }
  public CodeGenerator DefineMethodOverride(MethodInfo baseMethod, bool final)
  { MethodAttributes attrs = baseMethod.Attributes & ~(MethodAttributes.Abstract|MethodAttributes.NewSlot) |
                             MethodAttributes.HideBySig;
    if(final) attrs |= MethodAttributes.Final;
    ParameterInfo[] pis = baseMethod.GetParameters();
    MethodBuilder mb = TypeBuilder.DefineMethod(baseMethod.Name, attrs, baseMethod.ReturnType, GetParamTypes(pis));
    for(int i=0,offset=mb.IsStatic ? 0 : 1; i<pis.Length; i++)
      mb.DefineParameter(i+offset, pis[i].Attributes, pis[i].Name);

    // TODO: figure out how to use this properly
    //TypeBuilder.DefineMethodOverride(mb, baseMethod);
    return new CodeGenerator(this, mb);
  }

  public TypeGenerator DefineNestedType(string name, Type parent) { return DefineNestedType(0, name, parent); }
  public TypeGenerator DefineNestedType(TypeAttributes attrs, string name, Type parent)
  { if(nestedTypes==null) nestedTypes = CachedList<TypeGenerator>.Alloc();
    TypeAttributes ta = attrs | TypeAttributes.Class | TypeAttributes.NestedPublic;
    TypeGenerator ret = new TypeGenerator(Assembly, TypeBuilder.DefineNestedType(name, ta, parent));
    nestedTypes.Add(ret);
    return ret;
  }

  public CodeGenerator DefineProperty(string name, Type type)
  { return DefineProperty(MethodAttributes.Public, name, type, Type.EmptyTypes);
  }
  public CodeGenerator DefineProperty(MethodAttributes attrs, string name, Type type)
  { return DefineProperty(attrs, name, type, Type.EmptyTypes);
  }
  public CodeGenerator DefineProperty(string name, Type type, params Type[] paramTypes)
  { return DefineProperty(MethodAttributes.Public, name, type, paramTypes);
  }
  public CodeGenerator DefineProperty(MethodAttributes attrs, string name, Type type, params Type[] paramTypes)
  { PropertyBuilder pb = TypeBuilder.DefineProperty(name, PropertyAttributes.None, type, paramTypes);
    CodeGenerator cg = DefineMethod(attrs, "get_"+name, type, paramTypes);
    pb.SetGetMethod((MethodBuilder)cg.MethodBase);
    return cg;
  }

  public void DefineProperty(string name, Type type, out CodeGenerator get, out CodeGenerator set)
  { DefineProperty(MethodAttributes.Public, name, type, Type.EmptyTypes, out get, out set);
  }
  public void DefineProperty(MethodAttributes attrs, string name, Type type,
                             out CodeGenerator get, out CodeGenerator set)
  { DefineProperty(attrs, name, type, Type.EmptyTypes, out get, out set);
  }
  public void DefineProperty(string name, Type type, Type[] paramTypes, out CodeGenerator get, out CodeGenerator set)
  { DefineProperty(MethodAttributes.Public, name, type, paramTypes, out get, out set);
  }
  public void DefineProperty(MethodAttributes attrs, string name, Type type, Type[] paramTypes,
                             out CodeGenerator get, out CodeGenerator set)
  { PropertyBuilder pb = TypeBuilder.DefineProperty(name, PropertyAttributes.None, type, paramTypes);
    get = DefineMethod(attrs, "get_"+name, type, paramTypes);
    set = DefineMethod(attrs, "set_"+name, null, paramTypes);
    pb.SetGetMethod((MethodBuilder)get.MethodBase);
    pb.SetSetMethod((MethodBuilder)set.MethodBase);
  }

  public CodeGenerator DefinePropertyOverride(string name)
  { return DefinePropertyOverride(TypeBuilder.BaseType, name, PropertyOverride.Either, IsSealed);
  }
  public CodeGenerator DefinePropertyOverride(string name, PropertyOverride po)
  { return DefinePropertyOverride(TypeBuilder.BaseType, name, po, IsSealed);
  }
  public CodeGenerator DefinePropertyOverride(string name, bool final)
  { return DefinePropertyOverride(TypeBuilder.BaseType, name, PropertyOverride.Either, final);
  }
  public CodeGenerator DefinePropertyOverride(string name, PropertyOverride po, bool final)
  { return DefinePropertyOverride(TypeBuilder.BaseType, name, po, final);
  }
  public CodeGenerator DefinePropertyOverride(Type type, string name)
  { return DefinePropertyOverride(type.GetProperty(name), PropertyOverride.Either, IsSealed);
  }
  public CodeGenerator DefinePropertyOverride(Type type, PropertyOverride po, string name)
  { return DefinePropertyOverride(type.GetProperty(name), po, IsSealed);
  }
  public CodeGenerator DefinePropertyOverride(Type type, string name, bool final)
  { return DefinePropertyOverride(type.GetProperty(name), PropertyOverride.Either, final);
  }
  public CodeGenerator DefinePropertyOverride(Type type, string name, PropertyOverride po, bool final)
  { return DefinePropertyOverride(type.GetProperty(name), po, final);
  }
  public CodeGenerator DefinePropertyOverride(PropertyInfo baseProp)
  { return DefinePropertyOverride(baseProp, PropertyOverride.Either, IsSealed);
  }
  public CodeGenerator DefinePropertyOverride(PropertyInfo baseProp, PropertyOverride po)
  { return DefinePropertyOverride(baseProp, po, IsSealed);
  }
  public CodeGenerator DefinePropertyOverride(PropertyInfo baseProp, PropertyOverride po, bool final)
  { if(po==PropertyOverride.Either)
    { if(baseProp.CanRead && baseProp.CanWrite)
        throw new ArgumentException("This property has both a getter and a setter.");
    }
    else if(po==PropertyOverride.Read && !baseProp.CanRead)
      throw new InvalidOperationException("This property has no getter.");
    else if(po==PropertyOverride.Write && !baseProp.CanRead)
      throw new InvalidOperationException("This property has no getter.");

    return DefineMethodOverride(po==PropertyOverride.Read ? baseProp.GetGetMethod() :
                                po==PropertyOverride.Write ? baseProp.GetSetMethod() :
                                baseProp.CanRead ? baseProp.GetGetMethod() : baseProp.GetSetMethod(), final);
  }

  public void DefinePropertyOverride(string name, out CodeGenerator get, out CodeGenerator set)
  { DefinePropertyOverride(TypeBuilder.BaseType, name, IsSealed, out get, out set);
  }
  public void DefinePropertyOverride(string name, bool final, out CodeGenerator get, out CodeGenerator set)
  { DefinePropertyOverride(TypeBuilder.BaseType, name, final, out get, out set);
  }
  public void DefinePropertyOverride(Type type, string name, out CodeGenerator get, out CodeGenerator set)
  { DefinePropertyOverride(type.GetProperty(name), IsSealed, out get, out set);
  }
  public void DefinePropertyOverride(Type type, string name, bool final, out CodeGenerator get, out CodeGenerator set)
  { DefinePropertyOverride(type.GetProperty(name), final, out get, out set);
  }
  public void DefinePropertyOverride(PropertyInfo baseProp, out CodeGenerator get, out CodeGenerator set)
  { DefinePropertyOverride(baseProp, IsSealed, out get, out set);
  }
  public void DefinePropertyOverride(PropertyInfo baseProp, bool final, out CodeGenerator get, out CodeGenerator set)
  { get = baseProp.CanRead  ? DefineMethodOverride(baseProp.GetGetMethod(), final) : null;
    set = baseProp.CanWrite ? DefineMethodOverride(baseProp.GetSetMethod(), final) : null;
  }

  public Slot DefineStaticField(string name, Type type)
  { return DefineStaticField(FieldAttributes.Public, name, type);
  }
  public Slot DefineStaticField(FieldAttributes attrs, string name, Type type)
  { return new StaticSlot(TypeBuilder.DefineField(name, type, attrs|FieldAttributes.Static));
  }

  public CodeGenerator DefineStaticMethod(string name, Type retType, params Type[] paramTypes)
  { return DefineMethod(MethodAttributes.Public|MethodAttributes.Static, name, false, retType, paramTypes);
  }
  public CodeGenerator DefineStaticMethod(MethodAttributes attrs, string name, Type retType, params Type[] paramTypes)
  { return DefineMethod(attrs|MethodAttributes.Static, name, false, retType, paramTypes);
  }

  public Type FinishType()
  { if(initGen!=null)
    { initGen.EmitReturn();
      initGen.Finish();
    }

    Type ret = TypeBuilder.CreateType();

    if(nestedTypes!=null)
    { foreach(TypeGenerator tg in nestedTypes) tg.FinishType();
      nestedTypes.Dispose();
      nestedTypes = null;
    }
    if(constants!=null) { constants.Dispose(); constants=null; }
    if(constantSlots!=null) { constantSlots.Dispose(); constantSlots=null; }

    return ret;
  }

  public Slot DefineConstantSlot(Type type)
  { return new StaticSlot(TypeBuilder.DefineField("c$"+numConstants++, type,
                                                  FieldAttributes.Static|FieldAttributes.Private|FieldAttributes.InitOnly));
  }

  public Slot GetConstant(object value)
  { if(constants==null)
    { constants = new ConstantHelper();
      constantSlots = CachedList<Slot>.Alloc();
    }

    int index;
    if(!constants.GetIndex(value, out index)) return constantSlots[index];

    Type type = value.GetType();
    if(type.IsValueType) type=typeof(object);
    FieldBuilder fb = TypeBuilder.DefineField("c$"+numConstants++, type,
                                              FieldAttributes.Static|FieldAttributes.Private|FieldAttributes.InitOnly);
    Slot slot = new StaticSlot(fb);
    constantSlots.Add(slot);
    EmitConstantInitializer(value);
    initGen.EmitFieldSet(fb);
    return slot;
  }

  public CodeGenerator GetInitializer()
  { if(initGen==null) initGen = new CodeGenerator(this, TypeBuilder.DefineTypeInitializer());
    return initGen;
  }

  public readonly AssemblyGenerator Assembly;
  public readonly TypeBuilder TypeBuilder;

  void EmitConstantInitializer(object value)
  { CodeGenerator cg = GetInitializer();

    switch(Convert.GetTypeCode(value))
    { case TypeCode.Byte:   cg.EmitInt((int)(byte)value); goto box;
      case TypeCode.Char:   cg.EmitInt((int)(char)value); goto box;
      case TypeCode.Double: cg.ILG.Emit(OpCodes.Ldc_R8, (double)value); goto box;
      case TypeCode.Int16:  cg.EmitInt((int)(short)value); goto box;
      case TypeCode.Int32:  cg.EmitInt((int)value); goto box;
      case TypeCode.Int64:  cg.ILG.Emit(OpCodes.Ldc_I8, (long)value); goto box;
      case TypeCode.Object:
        if(value is Binding)
        { cg.EmitTopLevel();
          cg.EmitString(((Binding)value).Name);
          cg.EmitCall(typeof(TopLevel), "GetBinding");
        }
        else if(value is string[]) cg.EmitStringArray((string[])value);
        else if(value is object[]) cg.EmitObjectArray((object[])value);
        else if(value is MultipleValues)
        { cg.EmitObjectArray(((MultipleValues)value).Values);
          cg.EmitNew(typeof(MultipleValues), typeof(object[]));
        }
        else if(value is Complex)
        { Complex c = (Complex)value;
          cg.ILG.Emit(OpCodes.Ldc_R8, c.real);
          cg.ILG.Emit(OpCodes.Ldc_R8, c.imag);
          cg.EmitNew(typeof(Complex), typeof(double), typeof(double));
          goto box;
        }
        else if(!Options.Current.Language.EmitConstant(cg, value)) goto default;
        return;
      case TypeCode.SByte:  cg.EmitInt((int)(sbyte)value); goto box;
      case TypeCode.Single: cg.ILG.Emit(OpCodes.Ldc_R4, (float)value); goto box;
      case TypeCode.UInt16: cg.EmitInt((int)(ushort)value); goto box;
      case TypeCode.UInt32: cg.EmitInt((int)(uint)value); goto box;
      case TypeCode.UInt64: cg.ILG.Emit(OpCodes.Ldc_I8, (long)(ulong)value); goto box;
      default: throw new NotImplementedException("constant: "+value.GetType());
    }

    box: cg.ILG.Emit(OpCodes.Box, value.GetType());
  }

  Type[] GetParamTypes(ParameterInfo[] pi)
  { Type[] paramTypes = new Type[pi.Length];
    for(int i=0; i<pi.Length; i++) paramTypes[i] = pi[i].ParameterType;
    return paramTypes;
  }

  CachedList<TypeGenerator> nestedTypes;
  CachedList<Slot> constantSlots;
  ConstantHelper constants;
  CodeGenerator initGen;
  int numConstants;
}

} // namespace Scripting.Backend
