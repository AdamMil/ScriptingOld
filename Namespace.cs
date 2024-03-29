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
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

namespace Scripting.Backend
{

#region Namespace
public abstract class Namespace
{ public Namespace(Namespace parent, CodeGenerator cg)
  { Parent  = parent;
    codeGen = cg;
  }

  public virtual Slot AllocTemp(Type type)
  { throw new NotSupportedException("This namespace does not support temporaries.");
  }

  public void DeleteSlot(Name name)
  { Slot slot;
    if(!slots.TryGetValue(name, out slot))
    { if(Parent!=null) Parent.DeleteSlot(name);
      else EmitDelete(name, null);
    }
    else
    { EmitDelete(name, slot);
      if(name.Depth==Name.Local && slot is LocalSlot) codeGen.FreeLocalTemp(slot);
      slots.Remove(name);
    }
  }

  public Slot GetSlot(Name name) { return GetSlot(name, true); }
  public Slot GetSlot(Name name, bool makeIt)
  { if(name.Depth==Name.Global && Parent!=null) return Parent.GetSlot(name, true);
    Slot ret;
    if(!slots.TryGetValue(name, out ret))
    { if(Parent!=null) ret = Parent.GetSlot(name, false);
      // TODO: develop a way to communicate to this method whether or not this variable needs to be kept around
      if(ret==null && makeIt) slots[name] = ret = MakeSlot(name);
    }
    return ret;
  }

  public void RemoveSlot(Name name)
  { Slot slot = slots[name]; // implicit Contains() check
    if(name.Depth==Name.Local) codeGen.FreeLocalTemp(slot);
    slots.Remove(name);
  }

  public Namespace Parent;

  protected virtual void EmitDelete(Name name, Slot slot)
  { if(slot!=null)
    { if(slot.Type.IsValueType) codeGen.EmitZero(slot.Type);
      else codeGen.EmitFieldGet(typeof(Binding), "Unbound");
      slot.EmitSet(codeGen);
    }
  }

  protected virtual Slot MakeSlot(Name name)
  { return name.Depth==Name.Local ? codeGen.AllocLocalTemp(name.Type, true) : new EnvironmentSlot(name);
  }

  protected Dictionary<Name,Slot> slots = new Dictionary<Name,Slot>();
  protected CodeGenerator codeGen;
}
#endregion

#region ArrayNamespace
public sealed class ArrayNamespace : Namespace
{ public ArrayNamespace(Namespace parent, CodeGenerator cg, Slot arraySlot) : base(parent, cg) { Array = arraySlot; }

  public int ArraySize { get { return index; } }
  public override Slot AllocTemp(Type type) { return new ArraySlot(Array, index++, type); }

  readonly Slot Array;
  int index;
}
#endregion

#region FieldNamespace
public sealed class FieldNamespace : Namespace
{ public FieldNamespace(Namespace parent, string prefix, CodeGenerator cg) : base(parent, cg) { Prefix = prefix; }
  public FieldNamespace(Namespace parent, string prefix, CodeGenerator cg, Slot instance)
    : base(parent, cg) { Prefix=prefix; Instance=instance; }

  public override Slot AllocTemp(Type type)
  { return codeGen.TypeGenerator.DefineField(Prefix+"var$"+index.Next, type);
  }

  public readonly string Prefix;
  public readonly Slot Instance;

  static Index index = new Index();
}
#endregion

#region LocalNamespace
public sealed class LocalNamespace : Namespace
{ public LocalNamespace(Namespace parent, CodeGenerator cg) : base(parent, cg) { }
}
#endregion

#region TopLevelNamespace
public sealed class TopLevelNamespace : Namespace
{ public TopLevelNamespace(CodeGenerator cg) : base(null, cg) { TopSlot = new TopLevelSlot(); }
  public TopLevelNamespace(CodeGenerator cg, Slot top)
    : base(null, cg) { TopSlot = top==null ? new TopLevelSlot() : top; }

  public readonly Slot TopSlot;

  protected override void EmitDelete(Name name, Slot slot)
  { TopSlot.EmitGet(codeGen);
    codeGen.EmitString(name.String);
    codeGen.EmitFieldGet(typeof(Binding), "Unbound");
    codeGen.EmitCall(typeof(TopLevel), "Set");

    TopSlot.EmitGet(codeGen);
    codeGen.EmitString(name.String);
    codeGen.EmitCall(typeof(TopLevel), "Unbind");
  }

  protected override Slot MakeSlot(Name name)
  { if(name.Depth!=Name.Global) return base.MakeSlot(name);
    if(name.Type!=typeof(object)) throw new NotSupportedException("Global variables must be of type System.Object"); // TODO: can we relax this?
    return new NamedFrameSlot(TopSlot, name.String);
  }
}
#endregion

} // namespace Scripting.Backend
