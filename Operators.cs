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
using System.Reflection;
using System.Reflection.Emit;

// FIXME: the comparison operators short-circuit when Emitted but not when Evaluated. decide which way is more desirable

namespace Scripting
{

#region Operator
public abstract class Operator
{ protected Operator(string name, int min, int max) { Name=name; MinArgs=min; MaxArgs=max; }

  public void Emit(CodeGenerator cg, ref Type etype, params Node[] args) { Emit(Name, cg, ref etype, args); }
  public abstract void Emit(string name, CodeGenerator cg, ref Type etype, params Node[] args);

  public object Evaluate(params Node[] args) { return Evaluate(Name, args); }
  public object Evaluate(params object[] args) { return Evaluate(Name, args); }
  public virtual object Evaluate(string name, params Node[] args)
  { return Evaluate(name, Node.MakeObjectArray(args));
  }
  public virtual object Evaluate(string name, params object[] args) { throw new NotSupportedException(); }

  public virtual Type GetResultType() { return typeof(object); }

  public readonly string Name;
  public readonly int MinArgs, MaxArgs;

  protected void CheckArity(string name, Array args) { Ops.CheckArity(name, args.Length, MinArgs, MaxArgs); }

  public static readonly UnaryOperator BitwiseNot = new BitwiseNotOperator();
  public static readonly UnaryOperator LogicalNot = new LogicalNotOperator();
  public static readonly UnaryOperator UnaryMinus = new UnaryMinusOperator();

  public static readonly ComparisonOperator Equal = new EqualOperator();
  public static readonly ComparisonOperator NotEqual = new NotEqualOperator();
  public static readonly ComparisonOperator Identical = new IdenticalOperator();
  public static readonly ComparisonOperator NotIdentical = new NotIdenticalOperator();
  public static readonly ComparisonOperator Less = new LessOperator();
  public static readonly ComparisonOperator LessEqual = new LessEqualOperator();
  public static readonly ComparisonOperator More = new MoreOperator();
  public static readonly ComparisonOperator MoreEqual = new MoreEqualOperator();

  public static readonly BinaryOperator Add = new AddOperator();
  public static readonly BinaryOperator Subtract = new SubtractOperator();
  public static readonly BinaryOperator Multiply = new MultiplyOperator();
  public static readonly BinaryOperator Divide = new DivideOperator();
  public static readonly BinaryOperator FloorDivide = new FloorDivideOperator();
  public static readonly BinaryOperator Modulus = new ModulusOperator();
  public static readonly BinaryOperator Power = new PowerOperator();
  public static readonly BinaryOperator LeftShift = new LeftShiftOperator();
  public static readonly BinaryOperator RightShift = new RightShiftOperator();
  public static readonly BinaryOperator BitwiseAnd = new BitwiseAndOperator();
  public static readonly BinaryOperator BitwiseOr = new BitwiseOrOperator();
  public static readonly BinaryOperator BitwiseXor = new BitwiseXorOperator();

  public static readonly Operator Compare = new CompareOperator();
  public static readonly Operator LogicalAnd = new LogicalAndOperator();
  public static readonly Operator LogicalOr = new LogicalOrOperator();
}
#endregion

#region Unary Operators
#region UnaryOperator
public abstract class UnaryOperator : Operator
{ protected UnaryOperator(string name) : base(name, 1, 1) { }

  public override void Emit(string name, CodeGenerator cg, ref Type etype, params Node[] args)
  { CheckArity(name, args);
    EmitOp(cg, ref etype, args[0]);
  }
  
  public abstract void EmitOp(CodeGenerator cg, ref Type etype, Node node);
  
  public override object Evaluate(string name, params Node[] args)
  { CheckArity(name, args);
    return Evaluate(args[0].Evaluate());
  }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    return Evaluate(args[0]);
  }

  public abstract object Evaluate(object value);
}
#endregion

#region BitwiseNotOperator
public sealed class BitwiseNotOperator : UnaryOperator
{ internal BitwiseNotOperator() : base("bitwise not") { }

  public override void EmitOp(CodeGenerator cg, ref Type etype, Node node)
  { node.Emit(cg);
    cg.EmitCall(typeof(Ops), "BitwiseNegate", typeof(object));
    etype = typeof(object);
  }

  public override object Evaluate(object value) { return Ops.BitwiseNegate(value); }
}
#endregion

#region LogicalNotOperator
public sealed class LogicalNotOperator : UnaryOperator
{ public LogicalNotOperator() : base("logical not") { }

  public override void EmitOp(CodeGenerator cg, ref Type etype, Node node)
  { if(etype==typeof(void)) { node.EmitVoid(cg); return; }

    Type type = typeof(bool);
    node.Emit(cg, ref type);

    if(etype==typeof(bool))
    { if(type!=typeof(bool)) cg.EmitIsTrue(type);
      etype = typeof(CodeGenerator.negbool);
    }
    else
    { if(type!=typeof(bool) && type!=typeof(CodeGenerator.negbool)) cg.EmitIsTrue(type);
      cg.BoolToObject(type!=typeof(CodeGenerator.negbool));
      etype = typeof(object);
    }
  }

  public override object Evaluate(object value) { return Ops.FromBool(!Ops.IsTrue(value)); }

  public override Type GetResultType() { return typeof(bool); }
}
#endregion

#region UnaryMinusOperator
public sealed class UnaryMinusOperator : UnaryOperator
{ internal UnaryMinusOperator() : base("unary minus") { }

  public override void EmitOp(CodeGenerator cg, ref Type etype, Node node)
  { node.Emit(cg);
    cg.EmitCall(typeof(Ops), "Negate", typeof(object));
    etype = typeof(object);
  }

  public override object Evaluate(object value) { return Ops.Negate(value); }
}
#endregion
#endregion

#region Comparison Operators
#region CompareOperator
public sealed class CompareOperator : Operator
{ public CompareOperator() : base("comparison", 2, 2) { }

  public override void Emit(string name, CodeGenerator cg, ref Type etype, params Node[] args)
  { CheckArity(name, args);
    if(etype==typeof(void)) cg.EmitVoids(args);
    else
    { cg.EmitNodes(args[0], args[1]);
      cg.EmitCall(typeof(Ops), "Compare");
      if(!cg.TryEmitConvertTo(etype, typeof(int)))
      { cg.ILG.Emit(OpCodes.Box, typeof(int));
        etype = typeof(object);
      }
    }
  }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    return Ops.Compare(args[0], args[1]);
  }

  public override Type GetResultType() { return typeof(int); }
}
#endregion

#region ComparisonOperator
public abstract class ComparisonOperator : Operator
{ protected ComparisonOperator(string name) : base(name, 2, -1) { }

  public override void Emit(string name, CodeGenerator cg, ref Type etype, params Node[] args)
  { CheckArity(name, args);

    if(etype==typeof(void)) cg.EmitVoids(args);
    else
    { Slot a=null, b=null;
      bool keepAround = false;

      args[0].Emit(cg);
      if(args.Length==2)
      { if(!args[1].ClearsStack) args[1].Emit(cg);
        else
        { keepAround = args[1].Interrupts;
          a = cg.AllocLocalTemp(typeof(object), keepAround);
          a.EmitSet(cg);
          args[1].Emit(cg);
          b = cg.AllocLocalTemp(typeof(object));
          b.EmitSet(cg);
          a.EmitGet(cg);
          b.EmitGet(cg);
        }
        EmitOp(cg, ref etype);
      }
      else
      { Label f=cg.ILG.DefineLabel(), end=cg.ILG.DefineLabel();
        keepAround = Node.HasInterrupt(args);
        a = cg.AllocLocalTemp(typeof(object), keepAround);

        for(int i=1; i<args.Length; i++)
        { if(args[i].ClearsStack)
          { if(i==1) a.EmitSet(cg);
            args[i].Emit(cg);
            if(b==null) b = cg.AllocLocalTemp(typeof(object));
            b.EmitSet(cg);
            a.EmitGet(cg);
            b.EmitGet(cg);
          }
          else
          { if(i!=1) a.EmitGet(cg);
            args[i].Emit(cg);
          }

          if(i!=args.Length-1)
          { cg.ILG.Emit(OpCodes.Dup);
            a.EmitSet(cg);
          }

          Type type = typeof(bool);
          EmitOp(cg, ref type);
          cg.ILG.Emit(type==typeof(bool) ? OpCodes.Brfalse : OpCodes.Brtrue, f);
        }

        if(etype==typeof(bool)) cg.EmitBool(true);
        else
        { cg.EmitFieldGet(typeof(Ops), "TRUE");
          etype = typeof(object);
        }
        cg.ILG.Emit(OpCodes.Br_S, end);
        cg.ILG.MarkLabel(f);
        if(etype==typeof(bool)) cg.EmitBool(false);
        else cg.EmitFieldGet(typeof(Ops), "FALSE");
        cg.ILG.MarkLabel(end);
      }

      if(!keepAround && a!=null) cg.FreeLocalTemp(a);
      if(b!=null) cg.FreeLocalTemp(b);
    }
  }

  public abstract void EmitOp(CodeGenerator cg, ref Type etype);
  
  public override Type GetResultType() { return typeof(bool); }
}
#endregion

#region EqualOperator
public sealed class EqualOperator : ComparisonOperator
{ public EqualOperator() : base("equality") { }

  public override void EmitOp(CodeGenerator cg, ref Type etype)
  { if(etype.IsValueType)
    { cg.EmitCall(typeof(Ops), "AreEqual");
      cg.EmitConvertTo(etype, typeof(bool));
    }
    else
    { cg.EmitCall(typeof(Ops), "Equal");
      etype = typeof(object);
    }
  }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    for(int i=1; i<args.Length; i++) if(!Ops.AreEqual(args[i-1], args[i])) return Ops.FALSE;
    return Ops.TRUE;
  }
}
#endregion

#region NotEqualOperator
public sealed class NotEqualOperator : ComparisonOperator
{ public NotEqualOperator() : base("inequality") { }

  public override void EmitOp(CodeGenerator cg, ref Type etype)
  { if(etype==typeof(bool))
    { cg.EmitCall(typeof(Ops), "AreEqual");
      etype = typeof(CodeGenerator.negbool);
    }
    else if(etype.IsValueType)
    { cg.EmitCall(typeof(Ops), "AreEqual");
      cg.EmitLogicalNot();
      cg.EmitConvertTo(etype, typeof(bool));
    }
    else
    { cg.EmitCall(typeof(Ops), "NotEqual");
      etype = typeof(object);
    }
  }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    for(int i=1; i<args.Length; i++) if(Ops.AreEqual(args[i-1], args[i])) return Ops.FALSE;
    return Ops.TRUE;
  }
}
#endregion

#region IdenticalOperator
public sealed class IdenticalOperator : ComparisonOperator
{ public IdenticalOperator() : base("identity") { }

  public override void EmitOp(CodeGenerator cg, ref Type etype)
  { cg.ILG.Emit(OpCodes.Ceq);
    if(etype.IsValueType) cg.EmitConvertTo(etype, typeof(bool));
    else
    { cg.EmitCall(typeof(Ops), "FromBool");
      etype = typeof(object);
    }
  }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    for(int i=1; i<args.Length; i++) if(args[i-1]!=args[i]) return Ops.FALSE;
    return Ops.TRUE;
  }
}
#endregion

#region NotIdenticalOperator
public sealed class NotIdenticalOperator : ComparisonOperator
{ public NotIdenticalOperator() : base("non-identity") { }

  public override void EmitOp(CodeGenerator cg, ref Type etype)
  { cg.ILG.Emit(OpCodes.Ceq);
    if(etype==typeof(bool)) etype = typeof(CodeGenerator.negbool);
    else
    { cg.EmitLogicalNot();
      if(etype.IsValueType) cg.EmitConvertTo(etype, typeof(bool));
      else
      { cg.EmitCall(typeof(Ops), "FromBool");
        etype = typeof(object);
      }
    }
  }

  public override object Evaluate(string name, params Node[] args)
  { CheckArity(name, args);
    for(int i=1; i<args.Length; i++) if(args[i-1]==args[i]) return Ops.FALSE;
    return Ops.TRUE;
  }
}
#endregion

#region LessOperator
public sealed class LessOperator : ComparisonOperator
{ public LessOperator() : base("less than") { }

  public override void EmitOp(CodeGenerator cg, ref Type etype)
  { if(etype.IsValueType)
    { cg.EmitCall(typeof(Ops), "Compare");
      cg.EmitInt(0);
      cg.ILG.Emit(OpCodes.Clt);
      cg.EmitConvertTo(etype, typeof(bool));
    }
    else
    { cg.EmitCall(typeof(Ops), "Less");
      etype = typeof(object);
    }
  }

  public override object Evaluate(string name, object[] args)
  { CheckArity(name, args);
    for(int i=1; i<args.Length; i++) if(Ops.Compare(args[i-1], args[i])>=0) return Ops.FALSE;
    return Ops.TRUE;
  }
}
#endregion

#region LessEqualOperator
public sealed class LessEqualOperator : ComparisonOperator
{ public LessEqualOperator() : base("less than or equal") { }

  public override void EmitOp(CodeGenerator cg, ref Type etype)
  { if(etype.IsValueType)
    { cg.EmitCall(typeof(Ops), "Compare");
      cg.EmitInt(0);
      cg.ILG.Emit(OpCodes.Cgt);
      if(etype==typeof(bool)) etype = typeof(CodeGenerator.negbool);
      else
      { cg.EmitLogicalNot();
        cg.EmitConvertTo(etype, typeof(bool));
      }
    }
    else
    { cg.EmitCall(typeof(Ops), "LessEqual");
      etype = typeof(object);
    }
  }

  public override object Evaluate(string name, object[] args)
  { CheckArity(name, args);
    for(int i=1; i<args.Length; i++) if(Ops.Compare(args[i-1], args[i])>0) return Ops.FALSE;
    return Ops.TRUE;
  }
}
#endregion

#region MoreOperator
public sealed class MoreOperator : ComparisonOperator
{ public MoreOperator() : base("more than") { }

  public override void EmitOp(CodeGenerator cg, ref Type etype)
  { if(etype.IsValueType)
    { cg.EmitCall(typeof(Ops), "Compare");
      cg.EmitInt(0);
      cg.ILG.Emit(OpCodes.Cgt);
      cg.EmitConvertTo(etype, typeof(bool));
    }
    else
    { cg.EmitCall(typeof(Ops), "More");
      etype = typeof(object);
    }
  }

  public override object Evaluate(string name, object[] args)
  { CheckArity(name, args);
    for(int i=1; i<args.Length; i++) if(Ops.Compare(args[i-1], args[i])<=0) return Ops.FALSE;
    return Ops.TRUE;
  }
}
#endregion

#region MoreEqualOperator
public sealed class MoreEqualOperator : ComparisonOperator
{ public MoreEqualOperator() : base("greater than or equal") { }

  public override void EmitOp(CodeGenerator cg, ref Type etype)
  { if(etype==typeof(bool))
    { cg.EmitCall(typeof(Ops), "Compare");
      cg.EmitInt(0);
      cg.ILG.Emit(OpCodes.Clt);
      if(etype==typeof(bool)) etype = typeof(CodeGenerator.negbool);
      else
      { cg.EmitLogicalNot();
        cg.EmitConvertTo(etype, typeof(bool));
      }
    }
    else
    { cg.EmitCall(typeof(Ops), "MoreEqual");
      etype = typeof(object);
    }
  }

  public override object Evaluate(string name, object[] args)
  { CheckArity(name, args);
    for(int i=1; i<args.Length; i++) if(Ops.Compare(args[i-1], args[i])<0) return Ops.FALSE;
    return Ops.TRUE;
  }
}
#endregion
#endregion

#region Binary Operators
#region BinaryOperator
public abstract class BinaryOperator : Operator
{ protected BinaryOperator(string name) : base(name, 2, -1) { }
  protected BinaryOperator(string name, int minArgs) : base(name, minArgs, -1) { }
  protected BinaryOperator(string name, int minArgs, int maxArgs) : base(name, minArgs, maxArgs) { }

  public override void Emit(string name, CodeGenerator cg, ref Type etype, params Node[] args)
  { CheckArity(name, args);

    if(etype==typeof(void)) cg.EmitVoids(args);
    else
    { if(args.Length<2) EmitOp(cg, args);
      else
      { Slot a=null, b=null;
        bool keepAround = false;

        args[0].Emit(cg);
        for(int i=1; i<args.Length; i++)
        { if(!args[i].ClearsStack) args[i].Emit(cg);
          else
          { if(a==null)
            { keepAround = Node.HasInterrupt(args, i, args.Length-i);
              a = cg.AllocLocalTemp(typeof(object), keepAround);
            }
            a.EmitSet(cg);
            args[i].Emit(cg);
            if(b==null) b = cg.AllocLocalTemp(typeof(object));
            b.EmitSet(cg);
            a.EmitGet(cg);
            b.EmitGet(cg);
          }

          EmitOp(cg);
        }

        if(!keepAround && a!=null) cg.FreeLocalTemp(a);
        if(b!=null) cg.FreeLocalTemp(b);
      }

      etype = typeof(object);
    }
  }

  public abstract void EmitOp(CodeGenerator cg);

  public virtual void EmitOp(CodeGenerator cg, Node[] args)
  { if(args.Length==1) args[0].Emit(cg);
    else throw new NotSupportedException();
  }
}
#endregion

#region AddOperator
public sealed class AddOperator : BinaryOperator
{ public AddOperator() : base("addition", 1) { }

  public override void EmitOp(CodeGenerator cg) { cg.EmitCall(typeof(Ops), "Add"); }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    if(args.Length==1) return args[0];
    object ret = Ops.Add(args[0], args[1]);
    for(int i=2; i<args.Length; i++) ret = Ops.Add(ret, args[i]);
    return ret;
  }
}
#endregion

#region SubtractOperator
public sealed class SubtractOperator : BinaryOperator
{ public SubtractOperator() : base("subtraction", 1) { }

  public override void EmitOp(CodeGenerator cg) { cg.EmitCall(typeof(Ops), "Subtract"); }
  public override void EmitOp(CodeGenerator cg, Node[] args)
  { args[0].Emit(cg);
    cg.EmitCall(typeof(Ops), "Negate");
  }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    if(args.Length==1) return Ops.Negate(args[0]);
    object ret = Ops.Subtract(args[0], args[1]);
    for(int i=2; i<args.Length; i++) ret = Ops.Subtract(ret, args[i]);
    return ret;
  }
}
#endregion

#region MultiplyOperator
public sealed class MultiplyOperator : BinaryOperator
{ public MultiplyOperator() : base("multiplication", 1) { }

  public override void EmitOp(CodeGenerator cg) { cg.EmitCall(typeof(Ops), "Multiply"); }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    if(args.Length==1) return args[0];
    object ret = Ops.Multiply(args[0], args[1]);
    for(int i=2; i<args.Length; i++) ret = Ops.Multiply(ret, args[i]);
    return ret;
  }
}
#endregion

#region DivideOperator
public sealed class DivideOperator : BinaryOperator
{ public DivideOperator() : base("division", 1) { }

  public override void EmitOp(CodeGenerator cg) { cg.EmitCall(typeof(Ops), "Divide"); }
  public override void EmitOp(CodeGenerator cg, Node[] args)
  { cg.EmitConstantObject(1);
    args[0].Emit(cg);
    cg.EmitCall(typeof(Ops), "Divide");
  }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    object a, b;
    if(args.Length==1) { a=1; b=args[0]; }
    else { a=args[0]; b=args[1]; }
    a = Ops.Divide(a, b);
    for(int i=2; i<args.Length; i++) a = Ops.Divide(a, args[i]);
    return a;
  }
}
#endregion

#region FloorDivideOperator
public sealed class FloorDivideOperator : BinaryOperator
{ public FloorDivideOperator() : base("floor division", 1) { }

  public override void EmitOp(CodeGenerator cg) { cg.EmitCall(typeof(Ops), "FloorDivide"); }
  public override void EmitOp(CodeGenerator cg, Node[] args)
  { cg.EmitConstantObject(1);
    args[0].Emit(cg);
    cg.EmitCall(typeof(Ops), "FloorDivide");
  }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    object a, b;
    if(args.Length==1) { a=1; b=args[0]; }
    else { a=args[0]; b=args[1]; }
    a = Ops.FloorDivide(a, b);
    for(int i=2; i<args.Length; i++) a = Ops.FloorDivide(a, args[i]);
    return a;
  }
}
#endregion

#region ModulusOperator
public sealed class ModulusOperator : BinaryOperator
{ public ModulusOperator() : base("modulus", 2, 2) { }

  public override void EmitOp(CodeGenerator cg) { cg.EmitCall(typeof(Ops), "Modulus"); }

  public override object Evaluate(string name, params Node[] args)
  { CheckArity(name, args);
    return Ops.Modulus(args[0], args[1]);
  }
}
#endregion

#region PowerOperator
public sealed class PowerOperator : BinaryOperator
{ public PowerOperator() : base("exponentation", 2, 2) { }

  public override void EmitOp(CodeGenerator cg) { cg.EmitCall(typeof(Ops), "Power"); }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    return Ops.Power(args[0], args[1]);
  }
}
#endregion

#region LeftShiftOperator
public sealed class LeftShiftOperator : BinaryOperator
{ public LeftShiftOperator() : base("left shift", 2, 2) { }

  public override void EmitOp(CodeGenerator cg) { cg.EmitCall(typeof(Ops), "LeftShift"); }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    return Ops.LeftShift(args[0], args[1]);
  }
}
#endregion

#region RightShiftOperator
public sealed class RightShiftOperator : BinaryOperator
{ public RightShiftOperator() : base("right shift", 2, 2) { }

  public override void EmitOp(CodeGenerator cg) { cg.EmitCall(typeof(Ops), "RightShift"); }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    return Ops.RightShift(args[0], args[1]);
  }
}
#endregion

#region BitwiseAndOperator
public sealed class BitwiseAndOperator : BinaryOperator
{ public BitwiseAndOperator() : base("bitwise and", 1) { }

  public override void EmitOp(CodeGenerator cg) { cg.EmitCall(typeof(Ops), "BitwiseAnd"); }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    if(args.Length==1) return args[0];
    object ret = Ops.BitwiseAnd(args[0], args[1]);
    for(int i=2; i<args.Length; i++) ret = Ops.BitwiseAnd(ret, args[i]);
    return ret;
  }
}
#endregion

#region BitwiseOrOperator
public sealed class BitwiseOrOperator : BinaryOperator
{ public BitwiseOrOperator() : base("bitwise or", 1) { }

  public override void EmitOp(CodeGenerator cg) { cg.EmitCall(typeof(Ops), "BitwiseOr"); }

  public override object Evaluate(string name, params Node[] args)
  { CheckArity(name, args);
    if(args.Length==1) return args[0];
    object ret = Ops.BitwiseOr(args[0], args[1]);
    for(int i=2; i<args.Length; i++) ret = Ops.BitwiseOr(ret, args[i]);
    return ret;
  }
}
#endregion

#region BitwiseXorOperator
public sealed class BitwiseXorOperator : BinaryOperator
{ public BitwiseXorOperator() : base("bitwise xor", 1) { }

  public override void EmitOp(CodeGenerator cg) { cg.EmitCall(typeof(Ops), "BitwiseXor"); }

  public override object Evaluate(string name, params object[] args)
  { CheckArity(name, args);
    if(args.Length==1) return args[0];
    object ret = Ops.BitwiseXor(args[0], args[1]);
    for(int i=2; i<args.Length; i++) ret = Ops.BitwiseXor(ret, args[i]);
    return ret;
  }
}
#endregion

#region LogicalAndOperator
public sealed class LogicalAndOperator : Operator
{ public LogicalAndOperator() : base("logical and", 0, -1) { }

  public override void Emit(string name, CodeGenerator cg, ref Type etype, params Node[] args)
  { if(args.Length==0)
    { if(etype==typeof(bool)) { cg.EmitBool(true); return; }
      else if(etype!=typeof(void)) cg.EmitFieldGet(typeof(Ops), "TRUE");
    }
    else if(args.Length==1) args[0].Emit(cg);
    else
    { bool keepAround = Node.HasInterrupt(args);
      Slot tmp = cg.AllocLocalTemp(typeof(object), keepAround);
      Label done = cg.ILG.DefineLabel();
      for(int i=0; i<args.Length; i++)
      { Type type = typeof(bool);
        args[i].Emit(cg, ref type);
        if(type==typeof(bool) || type==typeof(CodeGenerator.negbool))
        { if(type==typeof(CodeGenerator.negbool)) cg.EmitLogicalNot();
          cg.ILG.Emit(OpCodes.Dup);
          cg.EmitCall(typeof(Ops), "FromBool");
          tmp.EmitSet(cg);
        }
        else
        { tmp.EmitSet(cg);
          cg.EmitIsTrue();
        }
        cg.ILG.Emit(OpCodes.Brfalse, done);
      }
      cg.ILG.MarkLabel(done);
      tmp.EmitGet(cg);
      if(!keepAround) cg.FreeLocalTemp(tmp);
    }
    etype = typeof(object);
  }
  
  public override object Evaluate(string name, params object[] args)
  { object ret = Ops.TRUE;
    for(int i=0; i<args.Length; i++)
    { if(!Ops.IsTrue(ret)) return ret;
      ret = args[i];
    }
    return ret;
  }
}
#endregion

#region LogicalOrOperator
public sealed class LogicalOrOperator : Operator
{ public LogicalOrOperator() : base("logical or", 0, -1) { }

  public override void Emit(string name, CodeGenerator cg, ref Type etype, params Node[] args)
  { if(args.Length==0)
    { if(etype==typeof(bool)) { cg.EmitBool(true); return; }
      else if(etype!=typeof(void)) cg.EmitFieldGet(typeof(Ops), "TRUE");
    }
    else if(args.Length==1) args[0].Emit(cg);
    else
    { bool keepAround = Node.HasInterrupt(args);
      Slot tmp = cg.AllocLocalTemp(typeof(object), keepAround);
      Label done = cg.ILG.DefineLabel();
      for(int i=0; i<args.Length; i++)
      { Type type = typeof(bool);
        args[i].Emit(cg, ref type);
        if(type==typeof(bool) || type==typeof(CodeGenerator.negbool))
        { if(type==typeof(CodeGenerator.negbool)) cg.EmitLogicalNot();
          cg.ILG.Emit(OpCodes.Dup);
          cg.EmitCall(typeof(Ops), "FromBool");
          tmp.EmitSet(cg);
        }
        else
        { tmp.EmitSet(cg);
          cg.EmitIsTrue();
        }
        cg.ILG.Emit(OpCodes.Brtrue, done);
      }
      cg.ILG.MarkLabel(done);
      tmp.EmitGet(cg);
      if(!keepAround) cg.FreeLocalTemp(tmp);
    }
    etype = typeof(object);
  }

  public override object Evaluate(string name, params object[] args)
  { object ret = Ops.FALSE;
    for(int i=0; i<args.Length; i++)
    { if(Ops.IsTrue(ret)) return ret;
      ret = args[i];
    }
    return ret;
  }
}
#endregion
#endregion

} // namespace Scripting