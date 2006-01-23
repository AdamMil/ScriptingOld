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
using System.Reflection;
using System.Reflection.Emit;

namespace Scripting.Backend
{

public class CodeGenerator
{ public CodeGenerator(TypeGenerator tg, MethodBase mb, ILGenerator ilg)
  { TypeGenerator=tg; MethodBase=mb; ILG=ilg;
  }

  public struct negbool { } // Used to represent the type of a boolean value that will end up being negated

  public Slot AllocLocalTemp(Type type) { return AllocLocalTemp(type, false); }
  public Slot AllocLocalTemp(Type type, bool keepAround)
  { CachedArray array = keepAround && IsGenerator ? nsTemps : localTemps;

    if(array!=null)
    { Slot slot;
      for(int i=0; i<array.Count; i++)
      { slot = (Slot)array[i];
        if(slot.Type==type)
        { array.RemoveAt(i);
          return slot;
        }
      }
    }

    return keepAround && IsGenerator ? Namespace.AllocTemp(type) : new LocalSlot(ILG.DeclareLocal(type));
  }

  public Slot AllocObjectArray(Node[] exprs) { return EmitObjectArray(exprs, 0, exprs.Length, true); }
  public Slot AllocObjectArray(Node[] exprs, int start, int length)
  { return EmitObjectArray(exprs, start, length, true);
  }

  public void BoolToObject() { BoolToObject(false); }
  public void BoolToObject(bool negate)
  { // TODO: possibly inline if optimizations are on?
    /*Label yes=ILG.DefineLabel(), end=ILG.DefineLabel();
    ILG.Emit(negate ? OpCodes.Brfalse_S : OpCodes.Brtrue_S, yes);
    EmitFieldGet(typeof(Ops), "FALSE");
    ILG.Emit(OpCodes.Br_S, end);
    ILG.MarkLabel(yes);
    EmitFieldGet(typeof(Ops), "TRUE");
    ILG.MarkLabel(end);*/
    
    if(negate)
    { EmitInt(0);
      ILG.Emit(OpCodes.Ceq);
    }
    EmitCall(typeof(Ops), "FromBool");
  }

  public void Dup() { ILG.Emit(OpCodes.Dup); }

  public void EmitArgGet(int index)
  { if(!MethodBase.IsStatic) index++;
    switch(index)
    { case 0: ILG.Emit(OpCodes.Ldarg_0); break;
      case 1: ILG.Emit(OpCodes.Ldarg_1); break;
      case 2: ILG.Emit(OpCodes.Ldarg_2); break;
      case 3: ILG.Emit(OpCodes.Ldarg_3); break;
      default: ILG.Emit(index<256 ? OpCodes.Ldarg_S : OpCodes.Ldarg, index); break;
    }
  }

  public void EmitArgGetAddr(int index)
  { if(!MethodBase.IsStatic) index++;
    ILG.Emit(index<256 ? OpCodes.Ldarga_S : OpCodes.Ldarga, index);
  }

  public void EmitArgSet(int index)
  { if(!MethodBase.IsStatic) index++;
    ILG.Emit(index<256 ? OpCodes.Starg_S : OpCodes.Starg, index);
  }

  public void EmitArrayLoad(Type type)
  { switch(Type.GetTypeCode(type))
    { case TypeCode.Boolean: case TypeCode.Byte: case TypeCode.SByte: ILG.Emit(OpCodes.Ldelem_I1); break;
      case TypeCode.Char: case TypeCode.Int16: case TypeCode.UInt16: ILG.Emit(OpCodes.Ldelem_I2); break;
      case TypeCode.Int32: case TypeCode.UInt32: ILG.Emit(OpCodes.Ldelem_I4); break;
      case TypeCode.Int64: case TypeCode.UInt64: ILG.Emit(OpCodes.Ldelem_I8); break;
      case TypeCode.Single: ILG.Emit(OpCodes.Ldelem_R4); break;
      case TypeCode.Double: ILG.Emit(OpCodes.Ldelem_R8); break;
      default:
        if(type.IsPointer || type==typeof(IntPtr)) ILG.Emit(OpCodes.Ldelem_I);
        else if(type.IsValueType)
        { ILG.Emit(OpCodes.Ldelema);
          ILG.Emit(OpCodes.Ldobj, type);
        }
        else ILG.Emit(OpCodes.Ldelem_Ref);
        break;
    }
  }

  public void EmitArrayStore(Type type)
  { switch(Type.GetTypeCode(type))
    { case TypeCode.Boolean: case TypeCode.Byte: case TypeCode.SByte: ILG.Emit(OpCodes.Stelem_I1); break;
      case TypeCode.Char: case TypeCode.Int16: case TypeCode.UInt16: ILG.Emit(OpCodes.Stelem_I2); break;
      case TypeCode.Int32: case TypeCode.UInt32: ILG.Emit(OpCodes.Stelem_I4); break;
      case TypeCode.Int64: case TypeCode.UInt64: ILG.Emit(OpCodes.Stelem_I8); break;
      case TypeCode.Single: ILG.Emit(OpCodes.Stelem_R4); break;
      case TypeCode.Double: ILG.Emit(OpCodes.Stelem_R8); break;
      default:
        if(type.IsPointer || type==typeof(IntPtr)) ILG.Emit(OpCodes.Stelem_I);
        else if(type.IsValueType)
        { ILG.Emit(OpCodes.Ldelema);
          ILG.Emit(OpCodes.Stobj, type);
        }
        else ILG.Emit(OpCodes.Stelem_Ref);
        break;
    }
  }

  public void EmitBool(bool value) { ILG.Emit(value ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0); }

  public void EmitCall(ConstructorInfo ci) { ILG.Emit(OpCodes.Call, ci); }
  public void EmitCall(MethodInfo mi)
  { if(mi.IsVirtual && !mi.DeclaringType.IsSealed && !mi.IsFinal) ILG.Emit(OpCodes.Callvirt, mi);
    else ILG.Emit(OpCodes.Call, mi);
  }
  public void EmitCall(Type type, string method) { EmitCall(type.GetMethod(method, SearchAll)); }
  public void EmitCall(Type type, string method, params Type[] paramTypes)
  { EmitCall(type.GetMethod(method, paramTypes));
  }

  public void EmitChar(char c) { EmitInt((int)c); }

  public void EmitConstant(object value)
  { switch(Convert.GetTypeCode(value))
    { case TypeCode.Boolean: EmitInt((bool)value ? 1 : 0); break;
      case TypeCode.Byte:   EmitInt((int)(byte)value); break;
      case TypeCode.Char:   EmitInt((int)(char)value); break;
      case TypeCode.Double: ILG.Emit(OpCodes.Ldc_R8, (double)value); break;
      case TypeCode.Empty:  ILG.Emit(OpCodes.Ldnull); break;
      case TypeCode.Int16:  EmitInt((int)(short)value); break;
      case TypeCode.Int32:  EmitInt((int)value); break;
      case TypeCode.Int64:  ILG.Emit(OpCodes.Ldc_I8, (long)value); break;
      case TypeCode.SByte:  EmitInt((int)(sbyte)value); break;
      case TypeCode.Single: ILG.Emit(OpCodes.Ldc_R4, (float)value); break;
      case TypeCode.String: EmitString((string)value); break;
      case TypeCode.UInt16: EmitInt((int)(ushort)value); break;
      case TypeCode.UInt32: EmitInt((int)(uint)value); break;
      case TypeCode.UInt64: ILG.Emit(OpCodes.Ldc_I8, (long)(ulong)value); break;
      default: throw new NotImplementedException("constant: "+value.GetType());
    }
  }

  public void EmitConstantObject(object value)
  { if(value==null) ILG.Emit(OpCodes.Ldnull);
    else if(value is bool) EmitFieldGet(typeof(Ops), (bool)value ? "TRUE" : "FALSE");
    else
    { string s = value as string;
      if(s!=null) EmitString(s);
      else TypeGenerator.GetConstant(value).EmitGet(this);
    }
  }

  public void EmitConvertTo(Type type, Type onStack) { EmitConvertTo(type, onStack, false); }
  public void EmitConvertTo(Type type, Type onStack, bool checkOverflow)
  { if(!TryEmitConvertTo(type, onStack, checkOverflow))
      throw new ArgumentException(onStack+" cannot be converted to "+type);
  }

  public void EmitDouble(double value) { ILG.Emit(OpCodes.Ldc_R8, value); }

  public void EmitFieldGet(Type type, string name) { EmitFieldGet(type.GetField(name, SearchAll)); }
  public void EmitFieldGet(FieldInfo field)
  { if(field.IsLiteral) EmitConstant(field.GetValue(null));
    else ILG.Emit(field.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld, field);
  }
  public void EmitFieldGetAddr(Type type, string name) { EmitFieldGetAddr(type.GetField(name, SearchAll)); }
  public void EmitFieldGetAddr(FieldInfo field)
  { if(field.IsLiteral) throw new ArgumentException("Cannot get the address of a literal field");
    ILG.Emit(field.IsStatic ? OpCodes.Ldsflda : OpCodes.Ldflda, field);
  }
  public void EmitFieldSet(Type type, string name) { EmitFieldSet(type.GetField(name, SearchAll)); }
  public void EmitFieldSet(FieldInfo field)
  { if(field.IsLiteral) throw new ArgumentException("Cannot set a literal field");
    ILG.Emit(field.IsStatic ? OpCodes.Stsfld : OpCodes.Stfld, field);
  }

  public void EmitDelete(Name name) { Namespace.DeleteSlot(name); }
  public void EmitGet(Name name) { Namespace.GetSlot(name).EmitGet(this); }
  public void EmitSet(Name name) { Namespace.GetSlot(name).EmitSet(this); }
  public void EmitSet(Name name, Slot value) { Namespace.GetSlot(name).EmitSet(this, value); }
  public void EmitSet(Name name, Node value) { Namespace.GetSlot(name).EmitSet(this, value); }

  public void EmitPropGet(Type type, string name) { EmitPropGet(type.GetProperty(name, SearchAll)); }
  public void EmitPropGet(PropertyInfo pi) { EmitCall(pi.GetGetMethod()); }
  public void EmitPropSet(Type type, string name) { EmitPropSet(type.GetProperty(name, SearchAll)); }
  public void EmitPropSet(PropertyInfo pi) { EmitCall(pi.GetSetMethod()); }

  public void EmitIndirectLoad(Type type)
  { if(!type.IsValueType) throw new ArgumentException("EmitIndirectLoad must be used with a value type");
    switch(Type.GetTypeCode(type))
    { case TypeCode.Boolean: case TypeCode.Byte: case TypeCode.SByte: ILG.Emit(OpCodes.Ldind_I1); break;
      case TypeCode.Int16: case TypeCode.UInt16: ILG.Emit(OpCodes.Ldind_I2); break;
      case TypeCode.Int32: case TypeCode.UInt32: ILG.Emit(OpCodes.Ldind_I4); break;
      case TypeCode.Int64: case TypeCode.UInt64: ILG.Emit(OpCodes.Ldind_I8); break;
      case TypeCode.Single: ILG.Emit(OpCodes.Ldind_R4); break;
      case TypeCode.Double: ILG.Emit(OpCodes.Ldind_R8); break;
      default:
        if(type.IsPointer || type==typeof(IntPtr)) ILG.Emit(OpCodes.Ldind_I);
        else ILG.Emit(OpCodes.Ldobj, type);
        break;
    }
  }

  public void EmitInt(int value)
  { OpCode op;
		switch(value)
		{ case -1: op=OpCodes.Ldc_I4_M1; break;
			case  0: op=OpCodes.Ldc_I4_0; break;
			case  1: op=OpCodes.Ldc_I4_1; break;
			case  2: op=OpCodes.Ldc_I4_2; break;
			case  3: op=OpCodes.Ldc_I4_3; break;
			case  4: op=OpCodes.Ldc_I4_4; break;
			case  5: op=OpCodes.Ldc_I4_5; break;
			case  6: op=OpCodes.Ldc_I4_6; break;
			case  7: op=OpCodes.Ldc_I4_7; break;
			case  8: op=OpCodes.Ldc_I4_8; break;
			default:
				if(value>=-128 && value<=127) ILG.Emit(OpCodes.Ldc_I4_S, (byte)value);
				else ILG.Emit(OpCodes.Ldc_I4, value);
				return;
		}
		ILG.Emit(op);
  }

  public void EmitIsFalse()
  { EmitIsTrue();
    EmitInt(0);
    ILG.Emit(OpCodes.Ceq);
  }

  public void EmitIsFalse(Type type)
  { if(type==typeof(object)) EmitIsFalse();
    else if(type==typeof(bool))
    { EmitInt(0);
      ILG.Emit(OpCodes.Ceq);
    }
    else if(type!=typeof(negbool))
    { ILG.Emit(OpCodes.Pop); // TODO: is this correct?
      EmitInt(type==null ? 1 : 0);
    }
  }

  public void EmitIsTrue() { Options.Current.Language.EmitIsTrue(this); }

  public void EmitIsTrue(Type type)
  { if(type==typeof(object)) EmitIsTrue();
    else if(type!=null)
    { ILG.Emit(OpCodes.Pop);
      EmitInt(1);
    }
  }

  public void EmitLanguage(Language lang) { EmitFieldGet(lang.GetType(), "Instance"); }

  public void EmitLogicalNot()
  { EmitInt(0);
    ILG.Emit(OpCodes.Ceq);
  }

  public void EmitNew(Type type) { EmitNew(type.GetConstructor(Type.EmptyTypes)); }
  public void EmitNew(Type type, params Type[] paramTypes)
  { EmitNew(type.GetConstructor(SearchAll, null, paramTypes, null));
  }
  public void EmitNew(ConstructorInfo ci) { ILG.Emit(OpCodes.Newobj, ci); }

  public void EmitNewArray(Type type, int length)
  { EmitInt(length);
    ILG.Emit(OpCodes.Newarr, type);
  }

  public void EmitNode(Node node)
  { if(node==null) ILG.Emit(OpCodes.Ldnull);
    else if(node.IsConstant)
    { EmitConstantObject(node.Evaluate());
      if(node.Tail) EmitReturn();
    }
    else node.Emit(this);
  }

  public void EmitNode(Node node, ref Type type)
  { if(node==null)
    { if(type!=typeof(void)) { ILG.Emit(OpCodes.Ldnull); type=null; }
    }
    else if(node.IsConstant)
    { if(type!=typeof(void)) Node.EmitConstant(this, node.Evaluate(), ref type);
      if(node.Tail) EmitReturn();
    }
    else node.Emit(this, ref type);
  }

  // FIXME: handle Interrupt nodes here and elsewhere
  public void EmitNodes(Node node1, Node node2) { EmitNodes(true, node1, node2); }
  public void EmitNodes(bool preserveOrder, Node node1, Node node2)
  { if(!node2.ClearsStack)
    { EmitNode(node1);
      EmitNode(node2);
    }
    else if(!preserveOrder)
    { node2.Emit(this);
      Slot tmp = AllocLocalTemp(typeof(object));
      tmp.EmitSet(this);
      node1.Emit(this);
      tmp.EmitGet(this);
      FreeLocalTemp(tmp);
    }
    else
    { node1.Emit(this);
      Slot a = AllocLocalTemp(typeof(object));
      a.EmitSet(this);
      node2.Emit(this);
      Slot b = AllocLocalTemp(typeof(object));
      b.EmitSet(this);
      a.EmitGet(this);
      b.EmitGet(this);
      FreeLocalTemp(a);
      FreeLocalTemp(b);
    }
  }
  
  public void EmitNodes(params Node[] nodes) { EmitNodes(true, nodes); }
  public void EmitNodes(bool preserveOrder, params Node[] nodes)
  { int clears = 0;
    bool beginning = true;

    for(int i=0; i<nodes.Length; i++)
      if(nodes[i]!=null && nodes[i].ClearsStack && clears++!=i) beginning = false;

    if(clears==0) foreach(Node n in nodes) if(n!=null) n.Emit(this);
    else if(clears>2 || preserveOrder && !beginning)
    { Slot arr = AllocObjectArray(nodes);
      for(int i=0; i<nodes.Length; i++)
      { arr.EmitGet(this);
        EmitInt(i);
        ILG.Emit(OpCodes.Ldelem_Ref);
      }
      FreeLocalTemp(arr);
    }
    else
    { Slot[] temps = new Slot[clears];
      int lastInterrupt = 0;
      bool keepAround = Node.HasInterrupt(nodes);

      for(int i=0,j=0; i<nodes.Length; i++)
        if(nodes[i]!=null)
        { if(nodes[i].Interrupts) lastInterrupt = j;
          if(nodes[i].ClearsStack)
          { temps[j] = AllocLocalTemp(typeof(object), keepAround);
            nodes[i].Emit(this);
            temps[j++].EmitSet(this);
          }
        }
      for(int i=0,j=0; i<nodes.Length; i++)
        if(nodes[i]==null || !nodes[i].ClearsStack) EmitNode(nodes[i]);
        else
        { temps[j].EmitGet(this);
          if(!keepAround) FreeLocalTemp(temps[j++]);
        }
    }
  }

  public void EmitNull() { ILG.Emit(OpCodes.Ldnull); }

  // TODO: skip over nodes that simply emit null
  public void EmitObjectArray(Node[] exprs) { EmitObjectArray(exprs, 0, exprs.Length, false); }
  public void EmitObjectArray(Node[] exprs, int start, int length) { EmitObjectArray(exprs, start, length, false); }
  public Slot EmitObjectArray(Node[] exprs, bool allocate)
  { return EmitObjectArray(exprs, 0, exprs.Length, allocate);
  }
  public Slot EmitObjectArray(Node[] exprs, int start, int length, bool allocate)
  { Slot arr = null;
    bool keepAround = Node.HasInterrupt(exprs, start, length);

    if(length==0) EmitFieldGet(typeof(Ops), "EmptyArray");
    else
    { Slot tmp=null;

      EmitNewArray(typeof(object), length);
      for(int i=start,end=start+length; i<end; i++)
      { if(exprs[i]!=null && exprs[i].ClearsStack)
        { if(arr==null)
          { arr = AllocLocalTemp(typeof(object[]), keepAround);
            arr.EmitSet(this);
          }
          exprs[i].Emit(this);
          if(tmp==null) tmp = AllocLocalTemp(typeof(object));
          tmp.EmitSet(this);
          arr.EmitGet(this);
        }

        if(arr==null || !exprs[i+1].ClearsStack) ILG.Emit(OpCodes.Dup);
        else if(i==end-1 && !allocate) ILG.Emit(OpCodes.Dup);
        EmitInt(i-start);

        if(exprs[i]==null || !exprs[i].ClearsStack) EmitNode(exprs[i]);
        else tmp.EmitGet(this);

        ILG.Emit(OpCodes.Stelem_Ref);
      }
      
      if(tmp!=null) FreeLocalTemp(tmp);
    }

    if(allocate)
    { if(arr==null) { arr=AllocLocalTemp(typeof(object[]), keepAround); arr.EmitSet(this); }
    }
    else if(!keepAround && arr!=null) { FreeLocalTemp(arr); arr=null; }
    return arr;
  }

  public void EmitObjectArray(object[] objs)
  { if(objs.Length==0) EmitFieldGet(typeof(Ops), "EmptyArray");
    else
    { EmitNewArray(typeof(object), objs.Length);
      for(int i=0; i<objs.Length; i++)
      { ILG.Emit(OpCodes.Dup);
        EmitInt(i);
        EmitConstantObject(objs[i]);
        ILG.Emit(OpCodes.Stelem_Ref);
      }
    }
  }

  public void EmitReturn() { ILG.Emit(OpCodes.Ret); }
  public void EmitReturn(Node expr)
  { EmitNode(expr);
    ILG.Emit(OpCodes.Ret);
  }

  public void EmitString(string value) // TODO: make sure this is storing strings in a string table
  { if(value==null) ILG.Emit(OpCodes.Ldnull);
    else ILG.Emit(OpCodes.Ldstr, value);
  }

  public void EmitStringArray(string[] strings)
  { EmitNewArray(typeof(string), strings.Length);
    for(int i=0; i<strings.Length; i++)
    { ILG.Emit(OpCodes.Dup);
      EmitInt(i);
      EmitString(strings[i]);
      ILG.Emit(OpCodes.Stelem_Ref);
    }
  }

  public void EmitThis()
  { if(MethodBase.IsStatic) throw new InvalidOperationException("no 'this' for a static method");
    ILG.Emit(OpCodes.Ldarg_0);
  }

  public void EmitTopLevel()
  { Namespace ns = Namespace;
    while(ns!=null && !(ns is TopLevelNamespace)) ns = ns.Parent;
    if(ns!=null) ((TopLevelNamespace)ns).TopSlot.EmitGet(this);
    else EmitFieldGet(typeof(TopLevel), "Current");
  }

  public void EmitTypeOf(Type type)
  { if(type.IsByRef) // TODO: see if there's a better way to do this (rather than calling GetType with a string). this might not even be safe for types in other assemblies (we may need to search through assemblies). maybe optimize it by caching values?
    { EmitString(type.FullName+"&");
      EmitCall(typeof(Type), "GetType", typeof(string));
    }
    else
    { ILG.Emit(OpCodes.Ldtoken, type);
      EmitCall(typeof(Type), "GetTypeFromHandle");
    }
  }

  public void EmitVoids(Node node1, Node node2)
  { if(node1!=null) node1.EmitVoid(this);
    if(node2!=null) node2.EmitVoid(this);
  }

  public void EmitVoids(params Node[] nodes) { foreach(Node n in nodes) if(n!=null) n.EmitVoid(this); }

  public void EmitZero(Type type)
  { if(!type.IsValueType) ILG.Emit(OpCodes.Ldnull);
    else if(type.IsPointer || type.GetElementType()!=null) throw new NotImplementedException("pointer types");
    else if(type==typeof(long) || type==typeof(ulong)) ILG.Emit(OpCodes.Ldc_I8, 0L);
    else if(type==typeof(float)) ILG.Emit(OpCodes.Ldc_R4, 0f);
    else if(type==typeof(double)) ILG.Emit(OpCodes.Ldc_R8, 0.0);
    else ILG.Emit(OpCodes.Ldc_I4_0);
  }

  public void Finish()
  { if(localTemps!=null) { localTemps.Dispose(); localTemps=null; }
    if(nsTemps!=null) { nsTemps.Dispose(); nsTemps=null; }
  }

  // TODO: go through the calls to this method and check whether the ones that are excluded due to an interrupt
  //       really need to be excluded
  public void FreeLocalTemp(Slot slot)
  { if(slot==null) throw new ArgumentException("Attempted to free a null temp slot.");
    if(slot is LocalSlot)
    { if(localTemps==null) localTemps = CachedArray.Alloc();
      localTemps.Add(slot);
    }
    else
    { if(nsTemps==null) nsTemps = CachedArray.Alloc();
      nsTemps.Add(slot);
    }
  }

  public void MarkPosition(Node node)
  { MarkPosition(node.StartPos.Line, node.StartPos.Column, node.EndPos.Line, node.EndPos.Column);
  }
  public void MarkPosition(int startLine, int startCol, int endLine, int endCol)
  { if(TypeGenerator.Assembly.IsDebug && startLine!=0 && endLine!=0)
      ILG.MarkSequencePoint(TypeGenerator.Assembly.Symbols, startLine, startCol, endLine, endCol);
  }

  public void SetupNamespace(int closedVars) { SetupNamespace(closedVars, null); }
  public void SetupNamespace(int closedVars, Slot topSlot)
  { Namespace = new TopLevelNamespace(this, topSlot);
    if(closedVars!=0) // we have a top level closure
    { Namespace = new LocalNamespace(Namespace, this);
      EmitArgGet(0);
      EmitInt(closedVars);
      EmitNew(typeof(LocalEnvironment), typeof(LocalEnvironment), typeof(int));
      EmitArgSet(0);
    }
  }

  public bool TryEmitConvertTo(Type type, Type onStack) { return TryEmitConvertTo(type, onStack, false); }
  public bool TryEmitConvertTo(Type type, Type onStack, bool checkOverflow)
  { if(type==null) throw new ArgumentNullException("type");
    if(onStack==type) return true;
    if(onStack==null) return !type.IsValueType;
    if(!type.IsValueType && type.IsAssignableFrom(onStack))
    { if(onStack.IsValueType) ILG.Emit(OpCodes.Box, onStack);
      return true;
    }

    if(onStack.IsPrimitive && type.IsPrimitive)
    { if(type==typeof(bool))
      { if(onStack==typeof(double) || onStack==typeof(float) || onStack==typeof(long)) ILG.Emit(OpCodes.Conv_I4);
        else if(onStack==typeof(ulong)) ILG.Emit(OpCodes.Conv_U4);
      }
      else if(type==typeof(sbyte))
        ILG.Emit(checkOverflow ? IsSigned(onStack) ? OpCodes.Conv_Ovf_I1 : OpCodes.Conv_Ovf_I1_Un : OpCodes.Conv_I1);
      else if(type==typeof(byte))
        ILG.Emit(checkOverflow ? IsSigned(onStack) ? OpCodes.Conv_Ovf_U1 : OpCodes.Conv_Ovf_U1_Un : OpCodes.Conv_U1);
      else if(type==typeof(short))
      { if(SizeOf(onStack)>=2)
          ILG.Emit(checkOverflow ? IsSigned(onStack) ? OpCodes.Conv_Ovf_I2 : OpCodes.Conv_Ovf_I2_Un : OpCodes.Conv_I2);
      }
      else if(type==typeof(ushort) || type==typeof(char))
      { if(SizeOf(onStack)>=2)
          ILG.Emit(checkOverflow ? IsSigned(onStack) ? OpCodes.Conv_Ovf_U2 : OpCodes.Conv_Ovf_U2_Un : OpCodes.Conv_U2);
      }
      else if(type==typeof(int))
      { if(SizeOf(onStack)>=4)
          ILG.Emit(checkOverflow ? IsSigned(onStack) ? OpCodes.Conv_Ovf_I4 : OpCodes.Conv_Ovf_I4_Un : OpCodes.Conv_I4);
      }
      else if(type==typeof(uint))
      { if(SizeOf(onStack)>=4)
          ILG.Emit(checkOverflow ? IsSigned(onStack) ? OpCodes.Conv_Ovf_U4 : OpCodes.Conv_Ovf_U4_Un : OpCodes.Conv_U4);
      }
      else if(type==typeof(long))
      { if(checkOverflow)
          ILG.Emit(onStack==typeof(ulong) ? OpCodes.Conv_Ovf_I8_Un :
                   onStack==typeof(float) || onStack==typeof(double) ? OpCodes.Conv_Ovf_I8 : OpCodes.Conv_I8);
        else if(onStack!=typeof(ulong)) ILG.Emit(OpCodes.Conv_I8);
      }
      else if(type==typeof(ulong))
      { if(checkOverflow) ILG.Emit(IsSigned(onStack) ? OpCodes.Conv_Ovf_U8 : OpCodes.Conv_U8);
        else if(onStack!=typeof(long)) ILG.Emit(OpCodes.Conv_U8);
      }
      else if(type==typeof(float)) ILG.Emit(OpCodes.Conv_R4);
      else if(type==typeof(double)) ILG.Emit(OpCodes.Conv_R8);
      else return false;

      return true;
    }

    if(onStack.IsValueType && type==typeof(object))
    { ILG.Emit(OpCodes.Box, onStack);
      return true;
    }

    MethodInfo mi = type.GetMethod("op_Implicit", BindingFlags.Public|BindingFlags.Static, null,
                                   new Type[] { onStack }, null);
    if(mi!=null && mi.ReturnType==type)
    { EmitCall(mi);
      return true;
    }

    ConstructorInfo ci = type.GetConstructor(BindingFlags.Public|BindingFlags.Instance, null, new Type[] { onStack },
                                             null);
    if(ci!=null)
    { EmitNew(ci);
      return true;
    }

    return false;
  }

  public Namespace Namespace;
  public bool IsGenerator;

  public readonly TypeGenerator TypeGenerator;
  public readonly MethodBase MethodBase;
  public readonly ILGenerator   ILG;

  const BindingFlags SearchAll = BindingFlags.Public|BindingFlags.NonPublic|BindingFlags.Instance|BindingFlags.Static;

  static bool IsSigned(Type type)
  { return type==typeof(int) || type==typeof(long) || type==typeof(short) || type==typeof(sbyte) ||
           type==typeof(float) || type==typeof(double);
  }

  static int SizeOf(Type type)
  { if(type==typeof(int) || type==typeof(uint) || type==typeof(float)) return 4;
    if(type==typeof(long) || type==typeof(ulong) || type==typeof(double)) return 8;
    if(type==typeof(short) || type==typeof(ushort) || type==typeof(char)) return 2;
    if(type==typeof(sbyte) || type==typeof(byte) || type==typeof(bool)) return 1;
    throw new NotSupportedException(type.FullName);
  }

  CachedArray localTemps, nsTemps;
}

} // namespace Scripting.Backend
