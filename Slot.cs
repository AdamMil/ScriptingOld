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

#region Slot
public abstract class Slot
{ public abstract Type Type { get; }

  public abstract void EmitGet(CodeGenerator cg);
  public abstract void EmitGetAddr(CodeGenerator cg);

  public abstract void EmitSet(CodeGenerator cg);
  public virtual void EmitSet(CodeGenerator cg, Slot value) { value.EmitGet(cg); EmitSet(cg); }
  public virtual void EmitSet(CodeGenerator cg, Node value) { EmitTypedNode(cg, value); EmitSet(cg); }

  protected void EmitTypedNode(CodeGenerator cg, Node node)
  { Type onStack = Type;
    node.Emit(cg, ref onStack);
    cg.EmitConvertTo(Type, onStack);
  }
}
#endregion

#region ArgSlot
public sealed class ArgSlot : Slot
{ public ArgSlot(MethodBuilder mb, int index, string name) : this(mb, index, name, typeof(object)) { }
  public ArgSlot(MethodBuilder mb, int index, string name, Type type)
    : this(mb, mb.DefineParameter(index+1, ParameterAttributes.None, name), type) { }
  public ArgSlot(MethodBase mb, ParameterBuilder parameterBuilder, Type type)
  { builder   = parameterBuilder;
    isStatic  = mb.IsStatic;
    this.type = type;
  }

  public override Type Type { get { return type; } }

  public override void EmitGet(CodeGenerator cg) { cg.EmitArgGet(builder.Position-1); }
  public override void EmitGetAddr(CodeGenerator cg) { cg.EmitArgGetAddr(builder.Position-1); }
  public override void EmitSet(CodeGenerator cg) { cg.EmitArgSet(builder.Position-1); }

  ParameterBuilder builder;
  Type type;
  bool isStatic;
}
#endregion

// TODO: eliminate the requirement to cast typed names on every retrieval
#region EnvironmentSlot
public sealed class EnvironmentSlot : Slot
{ public EnvironmentSlot(Name name) : this(name.Depth, name.Index, name.Type) { }
  public EnvironmentSlot(int depth, int index, Type type) { Depth=depth; Index=index; SlotType=type; }

  public override Type Type { get { return SlotType; } }

  public override void EmitGet(CodeGenerator cg)
  { GetArray(cg);
    cg.EmitInt(Index);
    cg.ILG.Emit(OpCodes.Ldelem_Ref);
    if(SlotType.IsValueType)
    { cg.ILG.Emit(OpCodes.Unbox, SlotType);
      if(SlotType.IsPrimitive || SlotType.IsPointer) cg.ILG.Emit(OpCodes.Ldobj, SlotType);
    }
    else if(SlotType!=typeof(object)) cg.ILG.Emit(OpCodes.Castclass, SlotType);
  }

  public override void EmitGetAddr(CodeGenerator cg)
  { if(SlotType!=typeof(object))
      throw new NotImplementedException("Getting the address of a non-Object variable is not supported");
    GetArray(cg);
    cg.EmitInt(Index);
    cg.ILG.Emit(OpCodes.Ldelema);
  }

  public override void EmitSet(CodeGenerator cg)
  { Slot temp = cg.AllocLocalTemp(typeof(object));
    temp.EmitSet(cg);
    EmitSet(cg, temp);
    cg.FreeLocalTemp(temp);
  }

  public override void EmitSet(CodeGenerator cg, Slot value)
  { GetArray(cg);
    cg.EmitInt(Index);
    value.EmitGet(cg);
    // TODO: check for compatibility between types
    if(value.Type.IsValueType) cg.ILG.Emit(OpCodes.Box, value.Type);
    cg.ILG.Emit(OpCodes.Stelem_Ref);
  }

  public override void EmitSet(CodeGenerator cg, Node value)
  { if(value.ClearsStack) base.EmitSet(cg, value);
    else
    { GetArray(cg);
      cg.EmitInt(Index);
      EmitTypedNode(cg, value);
      cg.ILG.Emit(OpCodes.Stelem_Ref);
    }
  }

  void GetArray(CodeGenerator cg)
  { if(Depth==0) cg.EmitArgGet(1);
    else
    { cg.EmitArgGet(0);
      for(int i=1; i<Depth; i++) cg.EmitFieldGet(typeof(LocalEnvironment), "Parent");
      cg.EmitFieldGet(typeof(LocalEnvironment), "Values");
    }
  }

  Type SlotType;
  int Index, Depth;
}
#endregion

#region FieldSlot
public sealed class FieldSlot : Slot
{ public FieldSlot(FieldInfo fi) { Info=fi; }
  public FieldSlot(Slot instance, FieldInfo fi) { Instance=instance; Info=fi; }

  public override Type Type { get { return Info.FieldType; } }

  public override void EmitGet(CodeGenerator cg)
  { if(Instance!=null) Instance.EmitGet(cg);
    cg.EmitFieldGet(Info);
  }

  public override void EmitGetAddr(CodeGenerator cg)
  { if(Instance!=null) Instance.EmitGet(cg);
    cg.EmitFieldGetAddr(Info);
  }

  public override void EmitSet(CodeGenerator cg)
  { if(Instance==null) cg.EmitFieldSet(Info);
    Slot temp = cg.AllocLocalTemp(Info.FieldType);
    temp.EmitSet(cg);
    EmitSet(cg, temp);
    cg.FreeLocalTemp(temp);
  }

  public override void EmitSet(CodeGenerator cg, Slot value)
  { if(Instance!=null) Instance.EmitGet(cg);
    value.EmitGet(cg);
    cg.EmitFieldSet(Info);
  }

  public override void EmitSet(CodeGenerator cg, Node value)
  { if(value.ClearsStack) base.EmitSet(cg, value);
    else
    { if(Instance!=null) Instance.EmitGet(cg);
      EmitTypedNode(cg, value);
      cg.EmitFieldSet(Info);
    }
  }

  public FieldInfo Info;
  public Slot Instance;
}
#endregion

#region TopLevelSlot
public sealed class TopLevelSlot : Slot
{ public override Type Type { get { return typeof(TopLevel); } }

  public override void EmitGet(CodeGenerator cg) { cg.EmitFieldGet(typeof(TopLevel), "Current"); }
  public override void EmitGetAddr(CodeGenerator cg) { cg.EmitFieldGetAddr(typeof(TopLevel), "Current"); }
  public override void EmitSet(CodeGenerator cg) { cg.EmitFieldSet(typeof(TopLevel), "Current"); }
}
#endregion

#region LocalSlot
public sealed class LocalSlot : Slot
{ public LocalSlot(LocalBuilder lb) { builder = lb; }
  public LocalSlot(LocalBuilder lb, string name)
  { builder = lb; 
    if(Options.Current.Debug) lb.SetLocalSymInfo(name);
  }

  public override Type Type { get { return builder.LocalType; } }

  public override void EmitGet(CodeGenerator cg) { cg.ILG.Emit(OpCodes.Ldloc, builder); }
  public override void EmitGetAddr(CodeGenerator cg) { cg.ILG.Emit(OpCodes.Ldloca, builder); }
  public override void EmitSet(CodeGenerator cg) { cg.ILG.Emit(OpCodes.Stloc, builder); }

  LocalBuilder builder;
}
#endregion

#region NamedFrameSlot
public sealed class NamedFrameSlot : Slot
{ public NamedFrameSlot(Slot frame, string name) { Frame=frame; Name=name; }

  public override Type Type { get { return typeof(object); } }

  public override void EmitGet(CodeGenerator cg)
  { SetupBinding(cg);
    Binding.EmitGet(cg);
    if(Options.Current.Debug) cg.EmitCall(typeof(Ops), "CheckBinding");
    cg.EmitFieldGet(typeof(Binding), "Value");
  }

  public override void EmitGetAddr(CodeGenerator cg)
  { SetupBinding(cg);
    Binding.EmitGet(cg);
    if(Options.Current.Debug) cg.EmitCall(typeof(Ops), "CheckBinding");
    cg.EmitFieldGetAddr(typeof(Binding), "Value");
  }

  public override void EmitSet(CodeGenerator cg)
  { Slot temp = cg.AllocLocalTemp(typeof(object));
    temp.EmitSet(cg);
    EmitSet(cg, temp);
    cg.FreeLocalTemp(temp);
  }

  public override void EmitSet(CodeGenerator cg, Slot value)
  { SetupBinding(cg);
    Binding.EmitGet(cg);
    if(Options.Current.Debug) cg.EmitCall(typeof(Ops), "CheckBinding");
    value.EmitGet(cg);
    cg.EmitFieldSet(typeof(Binding), "Value");
  }

  public override void EmitSet(CodeGenerator cg, Node value)
  { if(value.ClearsStack) base.EmitSet(cg, value);
    else
    { SetupBinding(cg);
      Binding.EmitGet(cg);
      if(Options.Current.Debug) cg.EmitCall(typeof(Ops), "CheckBinding");
      value.Emit(cg);
      cg.EmitFieldSet(typeof(Binding), "Value");
    }
  }

  public Slot Frame, Binding;
  public string Name;

  void SetupBinding(CodeGenerator cg)
  { if(Binding==null)
    { if(TopLevel.Current==null)
        throw new CompileTimeException("A top level environment is necessary to compile this code.");
      Binding = cg.TypeGenerator.GetConstant(TopLevel.Current.GetBinding(Name));
    }
  }
}
#endregion

#region StaticSlot
public sealed class StaticSlot : Slot
{ public StaticSlot(FieldInfo field) { this.field=field; }

  public override Type Type { get { return field.FieldType; } }

  public override void EmitGet(CodeGenerator cg) { cg.EmitFieldGet(field); }
  public override void EmitGetAddr(CodeGenerator cg) { cg.EmitFieldGetAddr(field); }
  public override void EmitSet(CodeGenerator cg) { cg.EmitFieldSet(field); }

  FieldInfo field;
}
#endregion

#region ThisSlot
public sealed class ThisSlot : Slot
{ public ThisSlot(Type type) { this.type=type; }

  public override Type Type { get { return type; } }

  public override void EmitGet(CodeGenerator cg) { cg.ILG.Emit(OpCodes.Ldarg_0); }
  public override void EmitGetAddr(CodeGenerator cg) { cg.ILG.Emit(OpCodes.Ldarga, 0); }
  public override void EmitSet(CodeGenerator cg) { cg.ILG.Emit(OpCodes.Starg, 0); }

  Type type;
}
#endregion

} // namespace Scripting.Backend
