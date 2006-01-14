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
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

// TODO: optimize compiler by caching methods, constructors, fields, properties, etc
// TODO: optimize code generation by not creating temp slots to hold the value of a node that's just a local variable
//       lookup or a constant expression
// TODO: implement some basic block analysis to detect and eliminate unreachable code
// TODO: possibly create a cache of commonly used data structures (eg ArrayLists) as well, so we don't have to create
//       so many of them everywhere
namespace Scripting
{

#region Argument
public enum ArgType { Normal, List, Dict };

public struct Argument
{ public Argument(Node expr) { Name=null; Expression=expr; Type=ArgType.Normal; }
  public Argument(string name, Node expr) { Name=name; Expression=expr; Type=ArgType.Normal; }
  public Argument(Node expr, ArgType type) { Name=null; Expression=expr; Type=type; }

  public string Name;
  public Node Expression;
  public ArgType Type;
}
#endregion

#region Attributes
public class DocStringAttribute : Attribute
{ public DocStringAttribute(string docs) { Docs=docs.Replace("\r\n", "\n"); }
  public string Docs;
}

[Flags] public enum RunAt { CompileTime=1, Runtime=2, Both=CompileTime|Runtime }

[AttributeUsage(AttributeTargets.Class|AttributeTargets.Struct, AllowMultiple=true)]
public class ScriptCodeAttribute : Attribute
{ public ScriptCodeAttribute(string code, Language language) { Code=code; Language=language; RunAt=RunAt.Runtime; }

  public readonly string Code;
  public readonly Language Language;
  public RunAt RunAt;
  public int Order;
}

public class ScriptNameAttribute : Attribute
{ public ScriptNameAttribute(string name) { Name=name; }
  public readonly string Name;
}
#endregion

#region Except
public struct Except
{ public Except(Node type, Node body) : this(null, type==null ? null : new Node[] { type }, body) { }
  public Except(Node[] types, Node body) : this(null, types, body) { }
  public Except(string var, Node type, Node body) : this(var, type==null ? null : new Node[] { type }, body) { }
  public Except(string var, Node[] types, Node body)
  { Var   = var==null ? null : new Name(var);
    Body  = body;
    Types = types==null || types.Length==0 ? null : types;
  }

  public Name Var;
  public readonly Node[] Types;
  public readonly Node Body;
}
#endregion

#region Exceptions
public abstract class ImplException : ApplicationException { }
public sealed class BreakException : ImplException
{ public BreakException(string name) { Name=name; }
  public readonly string Name;
}
public sealed class RestartException : ImplException
{ public RestartException(string name) { Name=name; }
  public readonly string Name;
}
#endregion

#region Index
public sealed class Index
{ public long Next { get { lock(this) return index++; } }
  long index;
}
#endregion

#region Language
public abstract class Language
{ static Language()
  { ops = new SortedList();
    foreach(string s in new string[] { "string[]", "object[]" }) ops[s] = null;

    ops["**%"] = typeof(Ops).GetMethod("PowerMod");
    ops["->string"] = typeof(Ops).GetMethod("Str");
    ops["==="] = Operator.Identical;
    ops["!=="] = Operator.NotIdentical;
    ops["&&"]  = Operator.LogicalAnd;
    ops["||"]  = Operator.LogicalOr;
    ops["!"]   = Operator.LogicalNot;
    ops["-"]   = Operator.Subtract;
    ops["~"]   = Operator.BitwiseNot;
    ops["+"]   = Operator.Add;
    ops["*"]   = Operator.Multiply;
    ops["/"]   = Operator.Divide;
    ops["//"]  = Operator.FloorDivide;
    ops["%"]   = Operator.Modulus;
    ops["&"]   = Operator.BitwiseAnd;
    ops["|"]   = Operator.BitwiseOr;
    ops["^"]   = Operator.BitwiseXor;
    ops["=="]  = Operator.Equal;
    ops["!="]  = Operator.NotEqual;
    ops["<"]   = Operator.Less;
    ops[">"]   = Operator.More;
    ops["<="]  = Operator.LessEqual;
    ops[">="]  = Operator.MoreEqual;
    ops["**"]  = Operator.Power;
    ops["<<"]  = Operator.LeftShift;
    ops[">>"]  = Operator.RightShift;

    constants = new string[ops.Count];
    ops.Keys.CopyTo(constants, 0);
    
    ops["object[]="] = null;
  }

  public virtual MemberContainer Builtins { get { return null; } }
  public virtual string BuiltinsNamespace { get { return null; } }
  public abstract string Name { get; }

  #region Ops
  #region Standard ops
  public virtual object Add(object a, object b)
  { throw Ops.TypeError("unsupported operand types for +: '{0}' and '{1}'", Ops.TypeName(a), Ops.TypeName(b));
  }
  public virtual object BitwiseAnd(object a, object b)
  { throw Ops.TypeError("unsupported operand types for &: '{0}' and '{1}'", Ops.TypeName(a), Ops.TypeName(b));
  }
  public virtual object BitwiseOr(object a, object b)
  { throw Ops.TypeError("unsupported operand types for |: '{0}' and '{1}'", Ops.TypeName(a), Ops.TypeName(b));
  }
  public virtual object BitwiseNegate(object a)
  { throw Ops.TypeError("unsupported operand type for ~: '{0}'", Ops.TypeName(a));
  }
  public virtual object BitwiseXor(object a, object b)
  { throw Ops.TypeError("unsupported operand types for ^: '{0}' and '{1}'", Ops.TypeName(a), Ops.TypeName(b));
  }
  public virtual int Compare(object a, object b)
  { throw Ops.TypeError("can't compare types: {0} and {1}", Ops.TypeName(a), Ops.TypeName(b));
  }
  public virtual object Divide(object a, object b)
  { throw Ops.TypeError("unsupported operand types for /: '{0}' and '{1}'", Ops.TypeName(a), Ops.TypeName(b));
  }
  public virtual object FloorDivide(object a, object b)
  { throw Ops.TypeError("unsupported operand types for //: '{0}' and '{1}'", Ops.TypeName(a), Ops.TypeName(b));
  }
  public virtual object LeftShift(object a, object b)
  { throw Ops.TypeError("unsupported operand types for <<: '{0}' and '{1}'", Ops.TypeName(a), Ops.TypeName(b));
  }
  public virtual object Modulus(object a, object b)
  { throw Ops.TypeError("unsupported operand types for %: '{0}' and '{1}'", Ops.TypeName(a), Ops.TypeName(b));
  }
  public virtual object Multiply(object a, object b)
  { throw Ops.TypeError("unsupported operand types for *: '{0}' and '{1}'", Ops.TypeName(a), Ops.TypeName(b));
  }
  public virtual object Negate(object a)
  { throw Ops.TypeError("unsupported operand type for unary -: '{0}'", Ops.TypeName(a));
  }
  public virtual object Power(object a, object b)
  { throw Ops.TypeError("unsupported operand types for **: '{0}' and '{1}'", Ops.TypeName(a), Ops.TypeName(b));
  }
  public virtual object RightShift(object a, object b)
  { throw Ops.TypeError("unsupported operand types for >>: '{0}' and '{1}'", Ops.TypeName(a), Ops.TypeName(b));
  }
  public virtual object Subtract(object a, object b)
  { throw Ops.TypeError("unsupported operand types for -: '{0}' and '{1}'", Ops.TypeName(a), Ops.TypeName(b));
  }
  #endregion
  
  #region Complex ops
  public virtual object ComplexAdd(Complex a, object b) { return Add(a, b); }
  public virtual object ComplexDivide(Complex a, object b) { return Divide(a, b); }
  public virtual object ComplexMultiply(Complex a, object b) { return Multiply(a, b); }
  public virtual object ComplexPower(Complex a, object b) { return Power(a, b); }
  public virtual object ComplexSubtract(Complex a, object b) { return Subtract(a, b); }
  #endregion
  
  #region Float ops
  public virtual object FloatAdd(double a, object b) { return Add(a, b); }
  public virtual int FloatCompare(double a, object b) { return Compare(a, b); }
  public virtual object FloatDivide(double a, object b) { return Divide(a, b); }
  public virtual object FloatFloorDivide(double a, object b) { return FloorDivide(a, b); }
  public virtual object FloatModulus(double a, object b) { return Modulus(a, b); }
  public virtual object FloatMultiply(double a, object b) { return Multiply(a, b); }
  public virtual object FloatPower(double a, object b) { return Power(a, b); }
  public virtual object FloatSubtract(double a, object b) { return Subtract(a, b); }
  #endregion

  #region Integer ops
  public virtual object IntegerAdd(Integer a, object b) { return Add(a, b); }
  public virtual object IntegerBitwiseAnd(Integer a, object b) { return BitwiseAnd(a, b); }
  public virtual object IntegerBitwiseOr(Integer a, object b) { return BitwiseOr(a, b); }
  public virtual object IntegerBitwiseNegate(Integer a) { return BitwiseNegate(a); }
  public virtual object IntegerBitwiseXor(Integer a, object b) { return BitwiseXor(a, b); }
  public virtual int IntegerCompare(Integer a, object b) { return Compare(a, b); }
  public virtual object IntegerDivide(Integer a, object b) { return Divide(a, b); }
  public virtual object IntegerFloorDivide(Integer a, object b) { return FloorDivide(a, b); }
  public virtual object IntegerLeftShift(Integer a, object b) { return LeftShift(a, b); }
  public virtual object IntegerModulus(Integer a, object b) { return Modulus(a, b); }
  public virtual object IntegerMultiply(Integer a, object b) { return Multiply(a, b); }
  public virtual object IntegerNegate(Integer a) { return Negate(a); }
  public virtual object IntegerPower(Integer a, object b) { return Power(a, b); }
  public virtual object IntegerRightShift(Integer a, object b) { return RightShift(a, b); }
  public virtual object IntegerSubtract(Integer a, object b) { return Subtract(a, b); }
  #endregion

  #region Int ops
  public virtual object IntAdd(int a, object b) { return Add(a, b); }
  public virtual object IntBitwiseAnd(int a, object b) { return BitwiseAnd(a, b); }
  public virtual object IntBitwiseOr(int a, object b) { return BitwiseOr(a, b); }
  public virtual object IntBitwiseNegate(int a) { return BitwiseNegate(a); }
  public virtual object IntBitwiseXor(int a, object b) { return BitwiseXor(a, b); }
  public virtual int IntCompare(int a, object b) { return Compare(a, b); }
  public virtual object IntDivide(int a, object b) { return Divide(a, b); }
  public virtual object IntFloorDivide(int a, object b) { return FloorDivide(a, b); }
  public virtual object IntLeftShift(int a, object b) { return LeftShift(a, b); }
  public virtual object IntModulus(int a, object b) { return Modulus(a, b); }
  public virtual object IntMultiply(int a, object b) { return Multiply(a, b); }
  public virtual object IntNegate(int a) { return Negate(a); }
  public virtual object IntPower(int a, object b) { return Power(a, b); }
  public virtual object IntRightShift(int a, object b) { return RightShift(a, b); }
  public virtual object IntSubtract(int a, object b) { return Subtract(a, b); }
  #endregion

  #region Long ops
  public virtual object LongAdd(long a, object b) { return Add(a, b); }
  public virtual object LongBitwiseAnd(long a, object b) { return BitwiseAnd(a, b); }
  public virtual object LongBitwiseOr(long a, object b) { return BitwiseOr(a, b); }
  public virtual object LongBitwiseNegate(long a) { return BitwiseNegate(a); }
  public virtual object LongBitwiseXor(long a, object b) { return BitwiseXor(a, b); }
  public virtual int LongCompare(long a, object b) { return Compare(a, b); }
  public virtual object LongDivide(long a, object b) { return Divide(a, b); }
  public virtual object LongFloorDivide(long a, object b) { return FloorDivide(a, b); }
  public virtual object LongLeftShift(long a, object b) { return LeftShift(a, b); }
  public virtual object LongModulus(long a, object b) { return Modulus(a, b); }
  public virtual object LongMultiply(long a, object b) { return Multiply(a, b); }
  public virtual object LongNegate(long a) { return Negate(a); }
  public virtual object LongPower(long a, object b) { return Power(a, b); }
  public virtual object LongRightShift(long a, object b) { return RightShift(a, b); }
  public virtual object LongSubtract(long a, object b) { return Subtract(a, b); }
  #endregion
  #endregion

  public virtual bool EmitConstant(CodeGenerator cg, object value) { return false; }
  public virtual void EmitIsTrue(CodeGenerator cg) { cg.EmitCall(typeof(Language), "DefaultIsTrue"); }

  public virtual void EmitNewKeywordDict(CodeGenerator cg)
  { cg.EmitNew(typeof(System.Collections.Specialized.ListDictionary), Type.EmptyTypes);
  }

  public virtual void EmitPackedArguments(CodeGenerator cg, Node[] args, int start, int length)
  { cg.EmitObjectArray(args, start, length);
  }

  // TODO: allow support for real name to be used in error messages
  #region EvaluateConstantFunction
  public virtual bool EvaluateConstantFunction(string name, Node[] args, out object result)
  { Operator op = ops[name] as Operator;
    if(op!=null) { result=op.Evaluate(name, args); return true; }
    if(!IsConstantFunction(name)) { result=null; return false; }

    object[] a = Node.MakeObjectArray(args);
    switch(name)
    { case "**%": Ops.CheckArity(name, a, 3); result = Ops.PowerMod(a[0], a[1], a[2]); break;
      case "->string": Ops.CheckArity(name, a, 1); result = Ops.Str(a[0]); break;

      case "string[]":
        Ops.CheckArity(name, a, 2);
        Ops.CheckType(name, a, 0, typeof(string));
        Ops.CheckType(name, a, 1, typeof(int));
        result = ((string)a[0])[Ops.ToInt(a[1])];
        break;

      case "object[]":
        Ops.CheckArity(name, a, 2);
        Ops.CheckType(name, a, 0, typeof(object[]));
        Ops.CheckType(name, a, 1, typeof(int));
        result = ((object[])a[0])[Ops.ToInt(a[1])];
        break;

      default: throw new NotImplementedException("unimplemented constant function: "+name);
    }

    return true;
  }
  #endregion

  public virtual bool ExcludeFromImport(string name) { return false; }

  public virtual string GenerateName(Node within, string baseName)
  { if(baseName==null || baseName=="") baseName = "g";
    return "#<" + baseName + GenNames.Next.ToString() + ">";
  }
  
  public virtual Type GetInlinedResultType(string functionName)
  { switch(functionName)
    { case "!": case "==": case "===": case "!=": case "!==": case "<": case "<=": case ">": case ">=":
        return typeof(bool);
      case "string[]": return typeof(char);
      default: return typeof(object);
    }
  }

  #region InlineFunction
  public virtual bool InlineFunction(CodeGenerator cg, string name, CallNode node, ref Type etype)
  { object op = ops[name];
    Node[] args = node.GetArgNodes();

    if(op is Operator)
    { ((Operator)op).Emit(name, cg, ref etype, args);
      return true;
    }

    switch(name)
    { case "->string":
        node.CheckArity(name, 1);
        if(etype==typeof(void)) { cg.EmitVoids(args); return true; }
        args[0].Emit(cg);
        break;

      case "**%":
        node.CheckArity(name, 3);
        if(etype==typeof(void)) { cg.EmitVoids(args); return true; }
        args[0].Emit(cg);
        args[1].Emit(cg);
        args[2].Emit(cg);
        break;

      case "string[]":
        node.CheckArity(2);
        if(etype==typeof(void)) { cg.EmitVoids(args); return true; }
        args[0].EmitString(cg);
        { Type type=typeof(int);
          args[1].Emit(cg, ref type);
          if(type!=typeof(int)) cg.EmitCall(typeof(Ops), "ToInt");
        }
        cg.EmitPropGet(typeof(string), "Chars");
        if(!etype.IsValueType || !cg.TryEmitConvertTo(etype, typeof(char)))
        { cg.ILG.Emit(OpCodes.Box, typeof(char));
          goto objret;
        }
        return true;

      case "object[]": case "object[]=":
        if(name=="object[]")
        { node.CheckArity(2);
          if(etype==typeof(void)) { cg.EmitVoids(args); return true; }
        }
        else node.CheckArity(3);
        args[0].EmitTyped(cg, typeof(object[]));
        { Type type = typeof(int);
          args[1].Emit(cg, ref type);
          if(type!=typeof(int)) cg.EmitCall(typeof(Ops), "ToInt");
        }
        if(name=="object[]") cg.ILG.Emit(OpCodes.Ldelem_Ref);
        else
        { args[2].Emit(cg);
          if(etype==typeof(void)) { cg.ILG.Emit(OpCodes.Stelem_Ref); return true; }
          else
          { cg.Dup();
            Slot tmp = cg.AllocLocalTemp(typeof(object));
            tmp.EmitSet(cg);
            cg.ILG.Emit(OpCodes.Stelem_Ref);
            tmp.EmitGet(cg);
            cg.FreeLocalTemp(tmp);
          }
        }
        goto objret;
      default: return false;
    }

    if(node.Tail && node.InTry==null) cg.ILG.Emit(OpCodes.Tailcall);
    cg.EmitCall((MethodInfo)op);
    objret:
    etype = typeof(object);
    return true;
  }
  #endregion

  public virtual bool IsConstantFunction(string name) { return Array.BinarySearch(constants, name)>=0; }
  public virtual bool IsHashableConstant(object value) { return false; }
  public virtual bool IsTrue(object value) { return DefaultIsTrue(value); }

  public virtual object PackArguments(object[] args, int start, int length)
  { if(start==0 && length==args.Length) return args;
    object[] ret = new object[length];
    Array.Copy(args, start, ret, 0, length);
    return ret;
  }

  public virtual MemberContainer LoadModule(Type type) { return ModuleGenerator.Generate(type); }

  public virtual CodeGenerator MakeCodeGenerator(TypeGenerator tg, MethodBase mb, ILGenerator ilg)
  { return new CodeGenerator(tg, mb, ilg);
  }

  public virtual IDictionary MakeKeywordDict() { return new System.Collections.Specialized.HybridDictionary(); }

  public Node Parse(string code) { return Parse("<unknown>", code); }

  public virtual Node Parse(string sourceName, string code)
  { return Parse(sourceName, new System.IO.StringReader(code));
  }

  public abstract Node Parse(string sourceName, System.IO.TextReader data);

  public Node ParseFile(string filename)
  { System.IO.StreamReader sr = new System.IO.StreamReader(filename);
    Node ret = Parse(System.IO.Path.GetFullPath(filename), sr);
    sr.Close();
    return ret;
  }

  public abstract string Repr(object obj);
  public abstract string Repr(Node node);

  public virtual bool ShouldAddBuiltins(Type type) { return true; }

  public virtual string Str(object obj)
  { TypeCode tc = Convert.GetTypeCode(obj);
    if(tc==TypeCode.Object) return Repr(obj);
    else if(tc==TypeCode.Empty) return "[NULL]";
    else return obj.ToString();
  }

  public virtual string TypeName(Type type) { return type.FullName; }

  public static bool DefaultIsTrue(object value) { return value!=null && (!(value is bool) || (bool)value); }

  readonly Index GenNames = new Index();

  static readonly SortedList ops;
  static readonly string[] constants;
}

public sealed class NullLanguage : Language
{ public override string Name { get { return "Null Language"; } }

  public override string GenerateName(Node within, string baseName)
  { throw new NotSupportedException("Null language has no syntax");
  }

  public override Node Parse(string sourceName, System.IO.TextReader data)
  { throw new NotSupportedException("Null language has no syntax");
  }

  public override string Str(object obj) { return obj==null ? "[NULL]" : obj.ToString(); }
  public override string Repr(object obj) { return Str(obj); }
  public override string Repr(Node node) { throw new NotSupportedException("Null language has no syntax"); }

  public static readonly NullLanguage Instance = new NullLanguage();
}
#endregion

#region MutatedName
public struct MutatedName
{ public MutatedName(Name name) { Name=name; Value=null; }
  public MutatedName(Name name, Node value) { Name=name; Value=value; }
  
  public Name Name;
  public readonly Node Value;
}
#endregion

#region Name
public sealed class Name
{ public Name(string name) { String=name; Depth=Local; Index=-1; Type=typeof(object); }
  public Name(string name, Type type) { String=name; Type=type; Depth=Local; Index=-1; }
  public Name(string name, int depth) { String=name; Depth=depth; Index=-1; Type=typeof(object); }
  public Name(string name, int depth, int index) { String=name; Depth=depth; Index=index; Type=typeof(object); }
  public Name(string name, int depth, int index, Type type) { String=name; Depth=depth; Index=index; Type=type; }

  public const int Global=-2, Local=-1;

  public override bool Equals(object obj) // these don't take Type into account (purposely)
  { Name o = obj as Name;
    return o!=null && o.String==String && o.Depth==Depth && o.Index==Index;
  }

  public override int GetHashCode() { return String.GetHashCode() ^ Depth ^ Index; }

  public readonly string String;
  public Type Type;
  public int Depth, Index;
}
#endregion

#region Options
public enum OptimizeType : byte { None, Size, Speed }

public sealed class Options
{ Options() { Language = NullLanguage.Instance; }

  public bool OptimizeAny { get { return Optimize!=OptimizeType.None; } }
  public bool OptimizeSize { get { return Optimize==OptimizeType.Size; } }
  public bool OptimizeSpeed { get { return Optimize==OptimizeType.Speed; } }

  public Language Language;
  public OptimizeType Optimize;
  public bool Debug, DebugModules, IsPreCompilation;
  
  public static Options Current
  { get { return Pushed==null || Pushed.Count==0 ? Default : (Options)Pushed.Peek(); }
  }

  public static void Restore() { Pushed.Pop(); }

  public static void Save()
  { if(Pushed==null) Pushed = new Stack();
    Pushed.Push(Current.MemberwiseClone());
  }

  public static Options Default = new Options();

  [ThreadStatic] static Stack Pushed;
}
#endregion

#region Parameter
public enum ParamType : byte { Required, Optional, List, Dict }

public struct Parameter
{ public Parameter(string name) : this(name, ParamType.Required) { }
  public Parameter(string name, Node defaultValue) : this(name, ParamType.Optional) { Default=defaultValue; }
  public Parameter(string name, ParamType type) { Name=new Name(name); Type=type; Default=null; Closed=false; }
  public Parameter(Name name, ParamType type, Node defaultValue)
  { Name=name; Type=type; Default=defaultValue; Closed=false;
  }

  public override int GetHashCode() { return Name.GetHashCode(); }

  public Name Name;
  public Node Default;
  public ParamType Type;
  public bool Closed;
  
  public static void CheckParms(Parameter[] parms, out int numRequired, out int optionalStart, out int numOptional,
                                out bool hasList, out bool hasDict)
  { ArrayList names = new ArrayList();
    bool os = false;
    numRequired = optionalStart = numOptional = 0;
    hasList = hasDict = false;

    for(int i=0; i<parms.Length; i++)
    { if(names.Contains(parms[i].Name.String))
        throw new ArgumentException("duplicate parameter: "+parms[i].Name.String);
      names.Add(parms[i].Name);
      switch(parms[i].Type)
      { case ParamType.Required:
          if(os) throw new ArgumentException("required parameters must precede optional parameters");
          numRequired++;
          break;
        case ParamType.Optional:
          if(hasList || hasDict) throw new ArgumentException("list and dictionary arguments must come last");
          if(os) numOptional++;
          else { optionalStart=i; numOptional=1; os=true; }
          break;
        case ParamType.List:
          if(hasDict) throw new ArgumentException("dictionary argument must come after list argument");
          if(hasList) throw new ArgumentException("multiple list arguments are not allowed");
          hasList = true;
          break;
        case ParamType.Dict:
          if(hasDict) throw new ArgumentException("multiple list arguments are not allowed");
          hasDict = true;
          break;
      }
    }
  }
  
  public static string[] GetNames(Parameter[] parms)
  { string[] names = new string[parms.Length];
    for(int i=0; i<names.Length; i++) names[i] = parms[i].Name.String;
    return names;
  }
}
#endregion

#region Position
public struct Position
{ public Position(int line, int column) { Line=line; Column=column; }
  public int Line, Column;
}
#endregion

#region Singleton
public sealed class Singleton
{ public Singleton(string name) { Name=name; }
  public override string ToString() { return "#<singleton: "+Name+">"; }
  public string Name;
}
#endregion

#region IWalker
public interface IWalker
{ void PostWalk(Node node);
  bool Walk(Node node);
}
#endregion

#region AST
public sealed class AST
{ AST() { }

  public static Node Create(Node body)
  { body.Preprocess();
    body.Walk(new NodeDecorator(null));
    return body;
  }

  public static LambdaNode CreateCompiled(Node body)
  { // wrapping it in a lambda node is done so we can keep the preprocessing code simple, and so that we can support
    // top-level closures. it's unwrapped later on by SnippetMaker.Generate() et al
    LambdaNode ret = new LambdaNode(body);
    ret.Preprocess();
    ret.Walk(new NodeDecorator(ret));
    return ret;
  }

  #region NodeDecorator
  /* This walker performs several tasks:
    1. Marks the containing ExceptionNode (InTry) of each node traversed, and the LambdaNode (InFunc) of each CallNode.
    2. Makes sure that interrupt nodes (nodes that exit the function and resume later (eg YieldNode)) do not occur
       within TryNodes
    3. Ensures that bare throw forms only occur within a catch block
    4. Resolves references to all names used within the function
    5. Sets node flags by calling node.SetFlags() [from the leaves to the root]
    6. Calls node.Optimize() if optimization is enabled [from the leaves to the root]
    7. Creates shared CachePromise objects for equivalent MemberNodes
    8. Sets the Parent fields of BlockNodes
    9. Finds all jump nodes (BreakNode and RestartNode) within blocks and updates them with information about whether
       they need to use the Leave opcode, etc.
    10. Determines whether blocks have early exits (an exit that comes before the block's tail) or restarts.
    11. Calls node.Postprocess()
  */
  sealed class NodeDecorator : IWalker
  { public NodeDecorator(LambdaNode top)
    { func = this.top = top;
      if(top!=null) { bound=new ArrayList(); free=new ArrayList(); values=new ArrayList(); }
      optimize = Options.Current.Optimize;
    }

    public bool Walk(Node node)
    { node.InTry = inTry;
      if(inTry!=null && node.Interrupts)
        throw Ops.SyntaxError(node, "An interrupt node is not valid within a try block.");

      if(node is CallNode) ((CallNode)node).InFunc = func;
      else if(node is LambdaNode)
      { foreach(Parameter p in func.Parameters) if(p.Default!=null) p.Default.Walk(this);

        LambdaNode oldFunc = func;
        ExceptionNode oldTry = inTry;
        bool oldCatch  = inCatch;
        func    = (LambdaNode)node;
        inTry   = null;
        inCatch = false;

        if(top==null) func.Body.Walk(this); // we don't need to resolve names in evaluated code
        else
        { int oldFree=freeStart, oldBound=boundStart, oldBlock=blockStart;
          ArrayList oldClosedParams=closedParams, oldClosedVars=closedVars;

          freeStart  = free.Count;
          boundStart = bound.Count;
          blockStart = blocks==null ? 0 : blocks.Count;
          closedVars = new ArrayList(); closedParams = new ArrayList();

          foreach(Parameter parm in func.Parameters)
          { bound.Add(parm.Name);
            values.Add(null);
          }

          func.Body.Walk(this);

          // if there are free variables in this function, try to resolve them
          for(int i=freeStart; i<free.Count; i++)
          { Name name = (Name)free[i];
            int index = IndexOf(name.String, bound, oldBound, boundStart);
            if(index==-1) // if it's not bound to any local variables of the previous function in the function stack...
            { if(oldFunc==top) name.Depth = Scripting.Name.Global; // it's global if oldFunc==top
              else
              { free[freeStart++] = name; // otherwise mark it for evaluation in the next frame up the stack
                name.Depth++;             // and increase its depth by one
              }
            }
            else // it's bound to a local variable in the previous function in the stack
            { Name bname = (Name)bound[index];
              int argPos = IndexOf(name.String, oldFunc.Parameters);
              if(argPos==-1 && bname.Depth==Scripting.Name.Local) // if it's not a parameter and it's local
              { bname.Depth = 1; // set its depth to one to indicate that it's coming from the LocalEnvironment
                bname.Index = name.Index = oldFunc.ClosedVars++; // and set the proper index, making sure to update
                bname.Type  = name.Type;                         // both instances of the name
                oldClosedVars.Add(name);
                oldClosedVars.Add(bname);
              }
              else if(bname.Depth==Scripting.Name.Global) { name.Depth = bname.Depth; continue; }
              else if(argPos!=-1)
              { name.Index = oldFunc.CloseParameter(argPos); // CloseParameter() sets bname.Depth
                oldClosedParams.Add(name);
              }
              else // the bound variable is already handled (or explicitly bound), so just update 'name'
              { name.Depth = bname.Depth;
                name.Index = bname.Index;
                name.Type  = bname.Type;
                oldClosedVars.Add(name);
              }

              LambdaNode lambda = values[index] as LambdaNode; // if the function is bound to a lambda throughout its
              if(lambda!=null) lambda.Binding = name;          // scope, we can use that info to optimize tail calls
            }
          }

          if(func.ClosedParams!=0)
          { // if the function closes parameters but not variables, the parameter indices will be wrong. fix them.
            if(func.ClosedVars==0)
            { for(int i=0; i<func.Parameters.Length; i++) func.Parameters[i].Name.Index = i;
              foreach(Name name in closedParams) name.Index = IndexOf(name.String, func.Parameters);
            }
            else // if it closes both parameters and variables, the indices of the variables will be wrong.
            { int numClosed = closedParams.Count;
              foreach(Name name in closedVars) name.Index += numClosed;
            }
          }

          values.RemoveRange(boundStart, bound.Count-boundStart);
          bound.RemoveRange(boundStart, bound.Count-boundStart);
          free.RemoveRange(freeStart, free.Count-freeStart);
          if(blocks!=null) blocks.RemoveRange(blockStart, blocks.Count-blockStart);
          boundStart=oldBound; freeStart=oldFree; blockStart=oldBlock;
          closedParams=oldClosedParams; closedVars=oldClosedVars;
        }
        func=oldFunc; inTry=oldTry; inCatch=oldCatch;
        return false;
      }
      else if(!inCatch && node is ThrowNode && ((ThrowNode)node).Exception==null)
        throw Ops.SyntaxError(node, "bare throw form is only allowed within a catch statement");
      else if(node is ExceptionNode)
      { if(tryBlocks==null) tryBlocks = new Stack();
        tryBlocks.Push(inTry);
        inTry = (ExceptionNode)node;

        if(node is TryNode)
        { TryNode tn=(TryNode)node;
          tn.Body.Walk(this);

          if(tn.Excepts!=null)
            foreach(Except ex in tn.Excepts)
            { if(ex.Types!=null) foreach(Node n in ex.Types) n.Walk(this);
              if(ex.Var!=null)
              { bound.Add(ex.Var);
                values.Add(null);
              }
              inCatch = true;
              ex.Body.Walk(this);
              inCatch = false;
              if(ex.Var!=null)
              { bound.RemoveAt(bound.Count-1);
                values.RemoveAt(values.Count-1);
              }
            }

          if(tn.Finally!=null) tn.Finally.Walk(this);
          return false;
        }
      }
      else if(top!=null) // compiled code only
      { if(node is VariableNode) HandleLocalReference(ref ((VariableNode)node).Name);
        else if(node is SetNodeBase)
        { SetNodeBase set = (SetNodeBase)node;
          MutatedName[] names = set.GetMutatedNames();
          bool updated = false;
          for(int i=0; i<names.Length; i++)
            if(HandleLocalReference(ref names[i].Name, names[i].Value, true)) updated = true;
          if(updated) set.UpdateNames(names);
        }
        else if(node is LocalBindNode)
        { LocalBindNode let = (LocalBindNode)node;
          foreach(Node n in let.Inits) if(n!=null) n.Walk(this);
          for(int i=0; i<let.Names.Length; i++)
          { bound.Add(let.Names[i]);
            values.Add(let.Inits[i]==null ? Scripting.Binding.Unbound : let.Inits[i]);
          }
          let.Body.Walk(this);
          return false;
        }
        else if(node is ValueBindNode)
        { ValueBindNode let = (ValueBindNode)node;
          foreach(Node n in let.Inits) n.Walk(this);
          foreach(Name[] names in let.Names)
            foreach(Name name in names) { bound.Add(name); values.Add(null); }
          let.Body.Walk(this);
          return false;
        }
        else if(node is JumpNode)
        { JumpNode jn = (JumpNode)node;
          if(blocks==null || blocks.Count==blockStart) throw Ops.SyntaxError(jn, "break/continue found outside block");
          int i;
          for(i=blocks.Count-1; i>=blockStart; i--)
          { BlockNode block = (BlockNode)blocks[i];
            if(block.Name==jn.Name)
            { if(jn is RestartNode)
              { RestartNode rn = (RestartNode)jn;
                rn.NeedsLeave = rn.InTry!=block.InTry;
                block.HasRestart = true;
              }
              else
              { BreakNode bn = (BreakNode)jn;
                bn.Block = block;

                if(!bn.Tail)
                  while(true)
                  { block.HasEarlyExit = true;
                    if(!block.Tail || block.Parent==null) break;
                    block = block.Parent;
                  }

                bn.NeedsLeave = bn.InTry!=block.InTry;
              }
              break;
            }
          }
          if(i==-1) throw Ops.SyntaxError(jn, "undefined block: "+jn.Name);
        }
        else if(node is BlockNode)
        { BlockNode bn = (BlockNode)node;
          if(blocks==null) blocks = new ArrayList();
          bn.Parent = blocks.Count==blockStart ? null : (BlockNode)blocks[blocks.Count-1];
          blocks.Add(bn);
        }
      }
      return true;
    }

    public void PostWalk(Node node)
    { if(node is ExceptionNode) inTry = (ExceptionNode)tryBlocks.Pop();
      if(top==null) return;

      if(node is LocalBindNode)
      { LocalBindNode let = (LocalBindNode)node;
        int len=let.Names.Length, start=bound.Count-len;

        for(int i=start; i<values.Count; i++)
        { LambdaNode lambda = values[i] as LambdaNode;
          if(lambda!=null) lambda.Binding = (Name)bound[i];
        }

        bound.RemoveRange(start, len);
        values.RemoveRange(start, len);
      }
      else if(node is ValueBindNode)
      { ValueBindNode let = (ValueBindNode)node;
        int len=0, start;
        foreach(Name[] names in let.Names) len += names.Length;
        start = bound.Count-len;
        bound.RemoveRange(start, len);
        values.RemoveRange(start, len);
      }
      else if(node is BlockNode) blocks.RemoveAt(blocks.Count-1);

      node.SetFlags();

      if(optimize!=OptimizeType.None)
      { node.Optimize();

        if(optimize==OptimizeType.Speed)
        { MemberNode an = node as MemberNode;
          if(an!=null && an.EnableCache && an.Value is VariableNode && an.Members.IsConstant)
          { object obj = an.Members.Evaluate();
            if(!(obj is string))
              throw new SyntaxErrorException("MemberNode expects a string value as the second argument");

            if(memberNodes==null) memberNodes = new Hashtable();
            AccessKey key = new AccessKey(((VariableNode)an.Value).Name, (string)obj);
            MemberNode.CachePromise promise = (MemberNode.CachePromise)memberNodes[key];
            if(promise==null) memberNodes[key] = promise = new MemberNode.CachePromise();
            an.Cache = promise;
          }
        }
      }

      node.Postprocess();
    }

    int IndexOf(string name, IList list) // these are kind of DWIMish
    { if(list==bound) return IndexOf(name, list, boundStart, list.Count);
      if(list==free)  return IndexOf(name, list, freeStart, list.Count);
      for(int i=0; i<list.Count; i++) if(((Parameter)list[i]).Name.String==name) return i;
      return -1;
    }

    bool HandleLocalReference(ref Name name) { return HandleLocalReference(ref name, null, false); }
    bool HandleLocalReference(ref Name name, Node assign) { return HandleLocalReference(ref name, assign, true); }
    bool HandleLocalReference(ref Name name, Node assign, bool useAssign)
    { int index = IndexOf(name.String, bound);
      if(index==-1)
      { if(func==top) name.Depth = Scripting.Name.Global;
        else
        { index = IndexOf(name.String, free);
          if(index==-1) { free.Add(name); name.Depth=1; }
          else { name = (Name)free[index]; return true; }
        }
      }
      else
      { name = (Name)bound[index];
        if(useAssign)
        { if(values[index]==Scripting.Binding.Unbound) values[index] = assign;
          else values[index] = null;
        }
        return true;
      }
      return false;
    }

    #region AccessKey
    struct AccessKey
    { public AccessKey(Name name, string members) { Name=name; Members=members; }

      public override bool Equals(object obj)
      { AccessKey other = (AccessKey)obj;
        return (Name==other.Name ||
                Name.Depth==Name.Global && other.Name.Depth==Name.Global && Name.String==other.Name.String) &&
               Members==other.Members;
      }

      public override int GetHashCode()
      { return Name.Depth ^ Name.Index ^ Name.String.GetHashCode() ^ Members.GetHashCode();
      }

      public Name Name;
      public string Members;
    }
    #endregion

    LambdaNode func, top;
    ExceptionNode inTry;
    Hashtable memberNodes;
    ArrayList blocks, bound, free, values, closedVars, closedParams;
    Stack tryBlocks;
    int boundStart, freeStart, blockStart;
    OptimizeType optimize;
    bool inCatch;

    static int IndexOf(string name, IList list, int start, int end)
    { for(end--; end>=start; end--) if(((Name)list[end]).String==name) return end;
      return -1;
    }
  }
  #endregion
}
#endregion

#region Node
public abstract class Node
{ [Flags] public enum Flag : byte { Tail=1, Const=2, ClearsStack=4, Interrupts=8 };

  public bool ClearsStack
  { get { return (Flags&Flag.ClearsStack) != 0; }
    set { if(value) Flags|=Flag.ClearsStack; else Flags&=~Flag.ClearsStack; }
  }

  public bool Interrupts
  { get { return (Flags&Flag.Interrupts) != 0; }
    set { if(value) Flags|=Flag.Interrupts; else Flags&=~Flag.Interrupts; }
  }

  public bool IsConstant
  { get { return (Flags&Flag.Const) != 0; }
    set { if(value) Flags|=Flag.Const; else Flags&=~Flag.Const; }
  }

  public bool Tail
  { get { return (Flags&Flag.Tail) != 0; }
    set { if(value) Flags|=Flag.Tail; else Flags&=~Flag.Tail; }
  }

  public void Emit(CodeGenerator cg)
  { Type type = typeof(object);
    Emit(cg, ref type);
    if(type.IsValueType) cg.EmitConvertTo(typeof(object), type);
  }

  public abstract void Emit(CodeGenerator cg, ref Type etype);

  public void EmitString(CodeGenerator cg) { EmitTyped(cg, typeof(string)); }

  public void EmitTyped(CodeGenerator cg, Type desired)
  { Type type = desired;
    Emit(cg, ref type);
    if(!AreEquivalent(type, desired))
    { if(!desired.IsValueType) cg.ILG.Emit(OpCodes.Castclass, desired);
      else
      { cg.ILG.Emit(OpCodes.Unbox, desired);
        cg.EmitIndirectLoad(desired);
      }
    }
  }

  public void EmitVoid(CodeGenerator cg)
  { if(!IsConstant)
    { Type type = typeof(void);
      Emit(cg, ref type);
      if(type!=typeof(void)) cg.ILG.Emit(OpCodes.Pop);
    }
  }

  public virtual object Evaluate() { throw new NotSupportedException(); }
  public string GenerateName(string baseName) { return Options.Current.Language.GenerateName(this, baseName); }
  public virtual Type GetNodeType() { return typeof(object); }
  public virtual void MarkTail(bool tail) { Tail=tail; }
  public virtual void Optimize() { }
  public void Preprocess() { MarkTail(true); }
  public virtual void SetFlags() { }
  public virtual void Postprocess() { }

  public virtual void Walk(IWalker w)
  { w.Walk(this);
    w.PostWalk(this);
  }

  public ExceptionNode InTry;
  public Position StartPos, EndPos;
  public Flag Flags;

  public static bool AreCompatible(Type type, Type desired)
  { if((type!=null && type.IsValueType) != (desired!=null && desired.IsValueType)) return false;
    Conversion conv = Ops.ConvertTo(type, desired);
    return conv!=Conversion.None && conv!=Conversion.Unsafe;
  }

  public static bool AreConstant(params Node[] nodes)
  { foreach(Node n in nodes) if(n!=null && !n.IsConstant) return false;
    return true;
  }

  public static bool AreEquivalent(Type type, Type desired)
  { Conversion conv = Ops.ConvertTo(type, desired);
    return conv==Conversion.Identity || conv==Conversion.Reference;
  }

  public static void EmitConstant(CodeGenerator cg, object value, ref Type etype)
  { if(etype==null) cg.EmitNull();
    else if(etype==typeof(void)) return;
    else
    { value = TryConvert(value, ref etype);
      if(etype.IsValueType)
      { if(Type.GetTypeCode(etype)!=TypeCode.Object) cg.EmitConstant(value);
        else
        { cg.EmitConstantObject(value);
          cg.ILG.Emit(OpCodes.Unbox, etype);
          cg.EmitIndirectLoad(etype);
        }
      }
      else cg.EmitConstantObject(value);
    }
  }

  public static bool HasInterrupt(params Node[] nodes) { return HasInterrupt(nodes, 0, nodes.Length); }
  public static bool HasInterrupt(Node[] nodes, int start, int length)
  { for(int i=0; i<length; i++)
    { Node n = nodes[i+start];
      if(n!=null && n.Interrupts) return true;
    }
    return false;
  }

  public static bool HasTryNode(params Node[] nodes)
  { if(nodes!=null) foreach(Node n in nodes) if(n!=null && n.ClearsStack) return true;
    return false;
  }

  public static object[] MakeObjectArray(Node[] nodes) { return MakeObjectArray(nodes, 0, nodes.Length); }
  public static object[] MakeObjectArray(Node[] nodes, int start, int length)
  { if(length==0) return Ops.EmptyArray;
    object[] ret = new object[length];
    for(int i=0; i<length; i++) ret[i] = nodes[i+start].Evaluate();
    return ret;
  }

  public static object TryConvert(object value, ref Type etype)
  { if(etype==null) return null;
    if(etype==typeof(object)) return value;

    Type vtype = value==null ? null : value.GetType();
    if(AreCompatible(vtype, etype))
    { value = Ops.ConvertTo(value, etype);
      etype = value.GetType();
    }
    else etype = value.GetType();
    return value;
  }

  // TODO: see if this and the related stuff can be implemented using CodeGenerator.EmitConvert
  protected static void EmitTryConvert(CodeGenerator cg, Type onStack, ref Type etype)
  { if(onStack==etype) return;
    if(onStack!=typeof(object) && !AreCompatible(onStack, etype))
      throw new InvalidOperationException(string.Format("Type mismatch: {0} var and {1} etype", onStack, etype));
    else if(!etype.IsValueType)
    { if(!onStack.IsValueType) etype = typeof(object);
      else
      { cg.ILG.Emit(OpCodes.Box, onStack);
        etype = onStack;
      }
    }
    else if(onStack==typeof(object)) etype = typeof(object);
    else etype = onStack;
  }

  protected static void EmitStrictConvert(CodeGenerator cg, Type onStack, Type destType)
  { if(destType.IsAssignableFrom(onStack))
    { if(onStack.IsValueType && !destType.IsValueType) cg.ILG.Emit(OpCodes.Box, onStack);
    }
    else if(destType.IsValueType==onStack.IsValueType)
    { if(!onStack.IsValueType) cg.ILG.Emit(OpCodes.Castclass, destType);
      else throw new NotImplementedException("Conversion between primitives"); // TODO: implement this
    }
    else throw new InvalidOperationException(string.Format("type mismatch: {0} and {1}", onStack, destType));
  }

  protected void TailReturn(CodeGenerator cg)
  { if(Tail)
    { if(InTry==null) cg.EmitReturn();
      else
      { InTry.ReturnSlot.EmitSet(cg);
        cg.ILG.Emit(OpCodes.Leave, InTry.LeaveLabel);
      }
    }
  }
}
#endregion

#region AssertNode
public sealed class AssertNode : WrapperNode
{ public AssertNode(Node expression) : this(expression, null) { }
  public AssertNode(Node expression, Node message)
  { string nodeText = "assertion failed: ";
    if(message==null) nodeText += Options.Current.Language.Repr(expression);

    Node text = new LiteralNode(nodeText);
    Node[] objs = message==null ? new Node[] { text } : new Node[] { text, message };
    // if not expression: throw AssertionError(repr(expression), message)
    Node = new IfNode(new UnaryOpNode(Operator.LogicalNot, expression),
                      new ThrowNode(new TypeNode(typeof(AssertionFailedException)), objs));
  }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(Options.Current.Debug) Node.Emit(cg, ref etype);
    else if(Tail) { cg.EmitNull(); TailReturn(cg); }
    else if(etype!=typeof(void)) { cg.EmitNull(); etype = typeof(object); }
  }
}
#endregion

#region BlockNode
public sealed class BlockNode : WrapperNode
{ public BlockNode(string name, Node body) : base(body) { Name = name; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(!Tail || HasEarlyExit) Node.MarkTail(false); // undo the peculiarity in MarkTail()

    // TODO: optimize to allow non-object blocks, assuming all returns are compatible
    Type returnType = etype==typeof(void) ? etype : typeof(object);

    bool hasReturnSlot = HasEarlyExit && etype!=typeof(void);
    bool usingParent = false;
    if(hasReturnSlot)
    { cg.EmitNull();
      if(Tail && Parent!=null)
      { ReturnSlot = Parent.ReturnSlot;
        usingParent = true;
      }
      else
      { ReturnSlot = cg.AllocLocalTemp(typeof(object), Node.Interrupts);
        ReturnSlot.EmitSet(cg);
      }
    }

    if(HasRestart)
    { StartLabel = cg.ILG.DefineLabel();
      cg.ILG.MarkLabel(StartLabel);
    }

    if(usingParent) EndLabel = Parent.EndLabel;
    else if(HasEarlyExit) EndLabel = cg.ILG.DefineLabel();

    if(HasEarlyExit || HasRestart) Walk(new JumpFinder(this));

    if(etype==typeof(void)) Node.EmitVoid(cg);
    else Node.Emit(cg, ref etype);

    if(etype!=returnType && returnType==typeof(void)) { cg.EmitNull(); returnType=etype=typeof(object); }
    else etype = returnType;

    if(!hasReturnSlot)
    { if(HasEarlyExit) cg.ILG.MarkLabel(EndLabel);
    }
    else
    { if(!usingParent)
      { ReturnSlot.EmitSet(cg);
        cg.ILG.MarkLabel(EndLabel);
        ReturnSlot.EmitGet(cg);
        if(!Node.Interrupts) cg.FreeLocalTemp(ReturnSlot);
      }
      ReturnSlot = null;
    }

    if(!Node.Tail) TailReturn(cg);
  }

  public override object Evaluate()
  { restart:
    try { return Node.Evaluate(); }
    catch(BreakException e) { if(e.Name!=Name) throw; return null; }
    catch(RestartException e) { if(e.Name==Name) goto restart; else throw; }
  }

  public override Type GetNodeType() { return typeof(object); } // TODO: change after optimization above

  public override void MarkTail(bool tail)
  { Node.MarkTail(true); // the body is marked as a tail even when it's not to facilitate optimizing out simple blocks
    Tail = tail;
  }

  public readonly string Name;
  public BlockNode Parent;
  public Slot ReturnSlot;
  public Label StartLabel, EndLabel;
  public bool HasRestart, HasEarlyExit;

  #region JumpFinder
  /* This walker performs the following tasks:
    1. Assign labels to jump nodes that reference this block
    2. Assigns to the BreakNode.Block field for BreakNodes that reference this block
  */
  sealed class JumpFinder : IWalker
  { public JumpFinder(BlockNode block) { this.block=block; }

    public bool Walk(Node node)
    { if(node is BreakNode)
      { BreakNode bn = (BreakNode)node;
        if(block.Name==bn.Name)
        { bn.Block = block;
          bn.Label = block.EndLabel;
        }
      }
      else if(node is RestartNode)
      { RestartNode rn = (RestartNode)node;
        if(block.Name==rn.Name) rn.Label = block.StartLabel;
      }
      else if(node is LambdaNode) return false;
      return true;
    }

    public void PostWalk(Node node) { }

    readonly BlockNode block;
  }
  #endregion
}
#endregion

#region BodyNode
public sealed class BodyNode : Node
{ public BodyNode(params Node[] forms) { Forms=forms; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(Forms.Length==0)
    { if(etype!=typeof(void))
      { cg.EmitNull();
        etype = null;
      }
    }
    else
    { int i;
      for(i=0; i<Forms.Length-1; i++) Forms[i].EmitVoid(cg);
      Forms[i].Emit(cg, ref etype);
    }
  }

  public override object Evaluate()
  { object ret = null;
    foreach(Node n in Forms) ret = n.Evaluate();
    return ret;
  }

  public override Type GetNodeType() { return Forms.Length==0 ? null : Forms[Forms.Length-1].GetNodeType(); }

  public override void MarkTail(bool tail)
  { for(int i=0; i<Forms.Length; i++) Forms[i].MarkTail(tail && i==Forms.Length-1);
    Tail = tail;
  }

  public override void Optimize()
  { bool isconst = true;
    foreach(Node n in Forms) if(!n.IsConstant) { isconst=false; break; }
    IsConstant = isconst;
  }

  public override void SetFlags()
  { ClearsStack = HasTryNode(Forms);
    Interrupts  = HasInterrupt(Forms);
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this)) foreach(Node n in Forms) n.Walk(w);
    w.PostWalk(this);
  }

  public readonly Node[] Forms;
}
#endregion

#region BreakNode
public sealed class BreakNode : JumpNode
{ public BreakNode(string name) : base(name) { }
  public BreakNode(string name, Node returnValue) : base(name) { Return = returnValue; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(Return==null) base.Emit(cg, ref etype);
    else if(Tail)
    { Return.Emit(cg, ref etype);
      TailReturn(cg);
    }
    else
    { if(Block.ReturnSlot==null) Return.EmitVoid(cg);
      else
      { Return.Emit(cg);
        Block.ReturnSlot.EmitSet(cg);
      }
      base.Emit(cg, ref etype);
    }
  }

  public override object Evaluate() { throw new BreakException(Name); }

  public override void Walk(IWalker w)
  { if(w.Walk(this) && Return!=null) Return.Walk(w);
    w.PostWalk(this);
  }

  public readonly Node Return;
  public BlockNode Block;
}
#endregion

#region CallNode
public sealed class CallNode : Node
{ public CallNode(Node func, params Node[] nodes) : this(func, NodesToArgs(nodes)) { }
  public CallNode(Node func, params Argument[] args)
  { Function=func; Args=args;

    int runlen = 0;
    foreach(Argument a in Args)
      switch(a.Type)
      { case ArgType.Normal:
          if(a.Name==null) runlen++;
          else NumNamed++;
          break;
        case ArgType.List:
          if(runlen!=0) { NumRuns++; runlen=0; }
          NumLists++;
          break;
        case ArgType.Dict: NumDicts++; break;
      }
    if(runlen!=0) NumRuns++;
    if(NumNamed!=0) NumRuns++;
    NumRuns += NumLists + NumDicts;
  }

  public void CheckArity(int num) { CheckArity(num, num); }
  public void CheckArity(int min, int max) { Ops.CheckArity(((VariableNode)Function).Name.String, Args.Length, min, max); }

  public void CheckArity(string name, int num) { Ops.CheckArity(name, Args.Length, num, num); }
  public void CheckArity(string name, int min, int max) { Ops.CheckArity(name, Args.Length, min, max); }

  #region Emit
  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(IsConstant)
    { EmitConstant(cg, Evaluate(), ref etype);
      TailReturn(cg);
      return;
    }

    cg.MarkPosition(this);

    bool hasTryArg = HasTryArg();
    if(hasTryArg || NumNamed!=0 || NumLists!=0 || NumDicts!=0) goto normal;

    if(Options.Current.OptimizeAny && Function is VariableNode)
    { VariableNode vn = (VariableNode)Function;
      if(Tail && FuncNameMatch(vn.Name, InFunc)) // see if we can tailcall ourselves with a branch
      { int positional = InFunc.Parameters.Length-(InFunc.HasList ? 1 : 0)-(InFunc.HasDict ? 1 : 0);
        if(Args.Length<positional)
          throw new TargetParameterCountException(
            string.Format("{0} expects {1}{2} args, but is being passed {3}",
                          vn.Name, InFunc.HasList ? "at least " : "", positional, Args.Length));
        // TODO: handle arguments that clear the stack
        for(int i=0; i<positional; i++) Args[i].Expression.Emit(cg);
        if(InFunc.HasList)
          Options.Current.Language.EmitPackedArguments(cg, GetArgNodes(), positional, Args.Length-positional);
        if(InFunc.HasDict) Options.Current.Language.EmitNewKeywordDict(cg);
        for(int i=InFunc.Parameters.Length-1; i>=0; i--) cg.EmitSet(InFunc.Parameters[i].Name);
        cg.ILG.Emit(InTry==null ? OpCodes.Br : OpCodes.Leave, InFunc.StartLabel);
        etype = typeof(object);
        return;
      }
      else if(Options.Current.Language.InlineFunction(cg, vn.Name.String, this, ref etype))
      { TailReturn(cg);
        return;
      }
    }

    normal:
    Slot ftmp=null, atmp=null;
    bool keepAround = HasInterruptArg();

    // FIXME: we need to preserve the left-to-right evaluation order
    Node[] nodes = GetArgNodes(false);
    if(NumLists==0 && NumDicts==0)
    { if(NumNamed==0)
      { Function.EmitTyped(cg, typeof(IProcedure));
        if(!hasTryArg) cg.EmitObjectArray(nodes);
        else
        { ftmp = cg.AllocLocalTemp(typeof(IProcedure), keepAround);
          ftmp.EmitSet(cg);
          atmp = cg.AllocObjectArray(nodes);
          ftmp.EmitGet(cg);
          atmp.EmitGet(cg);
        }
      }
      else
      { Function.EmitTyped(cg, typeof(IFancyProcedure));
        if(!hasTryArg)
        { cg.EmitObjectArray(nodes, 0, nodes.Length-NumNamed);
          EmitKeywordNames(cg);
          EmitKeywordValues(cg, false);
        }
        else
        { ftmp = cg.AllocLocalTemp(typeof(IFancyProcedure), keepAround);
          ftmp.EmitSet(cg);
          if(HasNamedTryArg())
          { hasTryArg = HasPlainTryArg();
            Slot values = EmitKeywordValues(cg, true);
            if(hasTryArg) atmp = cg.AllocObjectArray(nodes, 0, nodes.Length-NumNamed);

            ftmp.EmitGet(cg);
            if(hasTryArg) atmp.EmitGet(cg);
            else cg.EmitObjectArray(nodes, 0, nodes.Length-NumNamed);
            EmitKeywordNames(cg);
            values.EmitGet(cg);
            cg.FreeLocalTemp(values);
          }
          else
          { atmp = cg.AllocObjectArray(nodes, 0, nodes.Length-NumNamed);  
            ftmp.EmitGet(cg);
            atmp.EmitGet(cg);
            EmitKeywordNames(cg);
            EmitKeywordValues(cg, false);
          }
        }
      }

      if(Tail && InTry==null) cg.ILG.Emit(OpCodes.Tailcall);
      if(NumNamed==0) cg.EmitCall(typeof(IProcedure), "Call");
      else cg.EmitCall(typeof(IFancyProcedure), "Call", typeof(object[]), typeof(string[]), typeof(object[]));
    }
    else
    { int ri=0, rsi=0, runlen=0;

      Function.Emit(cg);
      if(hasTryArg)
      { ftmp = cg.AllocLocalTemp(typeof(object), keepAround);
        ftmp.EmitSet(cg);
      }

      cg.EmitNewArray(typeof(CallArg), NumRuns);
      if(hasTryArg) { atmp=cg.AllocLocalTemp(typeof(CallArg[]), keepAround); atmp.EmitSet(cg); }

      hasTryArg = false;
      for(int i=0; i<Args.Length; i++) // positional args
      { if(Args[i].Name!=null || Args[i].Type==ArgType.Dict) continue;
        if(Args[i].Type==ArgType.Normal)
        { if(Args[i].Expression.ClearsStack) hasTryArg = true;
          runlen++;
        }
        else
        { if(runlen!=0) EmitAndStoreRun(cg, atmp, ri++, nodes, hasTryArg, rsi, runlen, i);

          cg.Dup();
          cg.EmitInt(ri++);
          cg.ILG.Emit(OpCodes.Ldelema, typeof(CallArg));
          cg.Dup();
          Args[i].Expression.Emit(cg);
          cg.EmitFieldSet(typeof(CallArg), "Value");
          cg.EmitFieldGet(typeof(CallArg), "ListType");
          cg.EmitFieldSet(typeof(CallArg), "Type");

          runlen=0; rsi=i+1;
        }
      }
      if(runlen!=0) EmitAndStoreRun(cg, atmp, ri++, nodes, hasTryArg, rsi, runlen, Args.Length);
      
      if(NumNamed!=0) // then named arguments
      { Slot values;
        if(HasNamedTryArg()) values = EmitKeywordValues(cg, true);
        else values = null;

        if(atmp!=null) atmp.EmitGet(cg);
        else cg.Dup();
        cg.EmitInt(ri++);
        cg.ILG.Emit(OpCodes.Ldelema, typeof(CallArg));
        cg.Dup();
        EmitKeywordNames(cg);
        cg.EmitFieldSet(typeof(CallArg), "Value");
        if(values==null) EmitKeywordValues(cg, false);
        else
        { values.EmitGet(cg);
          cg.FreeLocalTemp(values);
        }
        cg.EmitFieldSet(typeof(CallArg), "Type");
      }
      
      for(int i=0; i<Args.Length; i++)
        if(Args[i].Type==ArgType.Dict)
        { Slot value;
          if(!Args[i].Expression.ClearsStack) value = null;
          else
          { Args[i].Expression.Emit(cg);
            value = cg.AllocLocalTemp(typeof(object), keepAround);
            value.EmitSet(cg);
          }
          
          if(atmp!=null) atmp.EmitGet(cg);
          else cg.Dup();
          cg.EmitInt(ri++);
          cg.ILG.Emit(OpCodes.Ldelema, typeof(CallArg));
          cg.Dup();
          if(value==null) Args[i].Expression.Emit(cg);
          else
          { value.EmitGet(cg);
            if(!keepAround) cg.FreeLocalTemp(value);
          }
          cg.EmitFieldSet(typeof(CallArg), "Value");
          cg.EmitFieldGet(typeof(CallArg), "DictType");
          cg.EmitFieldSet(typeof(CallArg), "Type");
        }
      
      if(ftmp!=null) ftmp.EmitGet(cg);
      if(atmp!=null) atmp.EmitGet(cg);
      if(Tail && InTry==null) cg.ILG.Emit(OpCodes.Tailcall);
      cg.EmitCall(typeof(Ops), "Call", typeof(object), typeof(CallArg[]));
    }

    TailReturn(cg);
    if(!keepAround)
    { if(ftmp!=null) cg.FreeLocalTemp(ftmp);
      if(atmp!=null) cg.FreeLocalTemp(atmp);
    }

    etype = typeof(object);
  }
  #endregion

  #region Evaluate
  public override object Evaluate()
  { if(IsConstant)
    { object result;
      if(Options.Current.Language.EvaluateConstantFunction(((VariableNode)Function).Name.String,
                                                           GetArgNodes(), out result))
        return result;
    }

    IProcedure func = Ops.ExpectProcedure(Function.Evaluate());
    if(Args.Length==0) func.Call(Ops.EmptyArray);
    
    Node[] nodes = GetArgNodes();
    if(NumLists==0 && NumDicts==0)
    { if(NumNamed==0) return func.Call(MakeObjectArray(nodes));
      else
      { object[] pos=new object[Args.Length-NumNamed], values=new object[NumNamed];
        string[] names = new string[NumNamed];
        for(int i=0,j=0,k=0; i<Args.Length; i++)
          if(Args[i].Name==null) pos[j++] = Args[i].Expression.Evaluate();
          else
          { names[k] = Args[i].Name;
            values[k++] = Args[i].Expression.Evaluate();
          }
        return Ops.ExpectFancyProcedure(func).Call(pos, names, values);
      }
    }
    else
    { int ri=0, rsi=0, runlen=0;
      CallArg[] cargs = new CallArg[NumRuns];
      
      for(int i=0; i<Args.Length; i++) // positional args
        if(Args[i].Name!=null || Args[i].Type==ArgType.Dict) continue;
        else if(Args[i].Type==ArgType.Normal) runlen++;
        else
        { if(runlen!=0) AddRun(cargs, ri++, nodes, rsi, runlen, i);
          cargs[ri++] = new CallArg(Args[i].Expression.Evaluate(), CallArg.ListType);
          runlen=0; rsi=i+1;
        }
      if(runlen!=0) AddRun(cargs, ri++, nodes, rsi, runlen, Args.Length);
      
      if(NumNamed!=0) // named arguments
      { string[] names = new string[NumNamed];
        object[] values = new object[NumNamed];
        for(int i=0,j=0; i<Args.Length; i++)
          if(Args[i].Name!=null)
          { names[j] = Args[i].Name;
            values[j++] = Args[i].Expression.Evaluate();
          }
        cargs[ri++] = new CallArg(names, values);
      }
      
      if(NumDicts!=0)
        for(int i=0; i<Args.Length; i++)
          if(Args[i].Type==ArgType.Dict)
            cargs[ri++] = new CallArg(Args[i].Expression.Evaluate(), CallArg.DictType);
    
      return Ops.Call(func, cargs);
    }
  }
  #endregion

  public Node[] GetArgNodes() { return GetArgNodes(true); }
  public Node[] GetArgNodes(bool throwOnComplexArg)
  { Node[] nodes = new Node[Args.Length];
    for(int i=0; i<Args.Length; i++)
      if(throwOnComplexArg && (Args[i].Type!=ArgType.Normal || Args[i].Name!=null))
        throw new ArgumentException("Unexpected list or dict argument");
      else nodes[i] = Args[i].Expression;
    return nodes;
  }

  #region GetNodeType
  public override Type GetNodeType()
  { if(Options.Current.OptimizeSpeed && !HasTryArg() && NumNamed==0 && NumLists==0 && NumDicts==0 &&
       Function is VariableNode)
    { string name = ((VariableNode)Function).Name.String;
      Language lang = Options.Current.Language;
      if(lang.IsConstantFunction(name)) return lang.GetInlinedResultType(name);
    }
    return typeof(object);
  }
  #endregion

  public override void MarkTail(bool tail)
  { Tail = tail;
    Function.MarkTail(false);
    foreach(Argument a in Args) a.Expression.MarkTail(false);
  }

  public override void Optimize()
  { bool isconst = true;
    foreach(Argument a in Args) if(!a.Expression.IsConstant) { isconst=false; break; }
    IsConstant = isconst && Function is VariableNode &&
                 Options.Current.Language.IsConstantFunction(((VariableNode)Function).Name.String);
  }

  public override void SetFlags()
  { bool clears=Function.ClearsStack, interrupts=Function.Interrupts;
    foreach(Argument a in Args)
    { if(a.Expression.ClearsStack) clears = true;
      if(a.Expression.Interrupts) interrupts = true;
    }
    ClearsStack = clears;
    Interrupts  = interrupts;
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this))
    { Function.Walk(w);
      foreach(Argument a in Args) a.Expression.Walk(w);
    }
    w.PostWalk(this);
  }

  public readonly Node Function;
  public readonly Argument[] Args;
  public readonly int NumLists, NumDicts, NumRuns, NumNamed;
  public LambdaNode InFunc;

  public static Argument[] NodesToArgs(Node[] nodes)
  { if(nodes==null) return null;
    Argument[] args = new Argument[nodes.Length];
    for(int i=0; i<nodes.Length; i++) args[i] = new Argument(nodes[i]);
    return args;
  }

  void AddRun(CallArg[] cargs, int ai, Node[] nodes, int start, int length, int end)
  { if(length!=1) cargs[ai] = new CallArg(MakeObjectArray(nodes, start, length), length);
    else
      for(; start<end; start++)
        if(Args[start].Name==null) cargs[ai] = new CallArg(Args[start].Expression.Evaluate(), null);
  }

  void EmitAndStoreRun(CodeGenerator cg, Slot atmp, int ai, Node[] nodes, bool hasTryArg,
                       int start, int length, int end)
  { Slot value;

    if(length==1) for(; start<end; start++) if(Args[start].Name==null) break;

    bool keepAround = false;
    if(!hasTryArg) value = null;
    else
    { if(length==1) Args[start].Expression.Emit(cg);
      else cg.EmitObjectArray(nodes, start, length);
      keepAround = length==1 ? Args[start].Expression.Interrupts : Node.HasInterrupt(nodes, start, length);
      value = cg.AllocLocalTemp(typeof(object), keepAround);
      value.EmitSet(cg);
    }

    if(atmp!=null) atmp.EmitGet(cg);
    else cg.Dup();

    cg.EmitInt(ai);
    cg.ILG.Emit(OpCodes.Ldelema, typeof(CallArg));
    cg.Dup();

    if(hasTryArg)
    { value.EmitGet(cg);
      if(!keepAround) cg.FreeLocalTemp(value);
    }
    else if(length==1) Args[start].Expression.Emit(cg);
    else cg.EmitObjectArray(nodes, start, length);

    cg.EmitFieldSet(typeof(CallArg), "Value");
    if(length==1) cg.EmitNull();
    else cg.EmitConstantObject(length);
    cg.EmitFieldSet(typeof(CallArg), "Type");
  }

  void EmitKeywordNames(CodeGenerator cg)
  { cg.EmitNewArray(typeof(string), NumNamed);
    for(int i=0,j=0; i<Args.Length; i++)
      if(Args[i].Name!=null)
      { cg.Dup();
        cg.EmitInt(j++);
        cg.EmitString(Args[i].Name);
        cg.ILG.Emit(OpCodes.Stelem_Ref);
      }
  }

  Slot EmitKeywordValues(CodeGenerator cg, bool allocate)
  { Node[] nodes = new Node[NumNamed];
    for(int i=0,j=0; i<Args.Length; i++)
      if(Args[i].Name!=null) nodes[j++] = Args[i].Expression;
    return cg.EmitObjectArray(nodes, allocate);
  }

  bool HasInterruptArg()
  { for(int i=0; i<Args.Length; i++) if(Args[i].Expression.Interrupts) return true;
    return false;
  }

  bool HasNamedTryArg()
  { for(int i=0; i<Args.Length; i++)
      if(Args[i].Name!=null && Args[i].Expression.ClearsStack) return true;
    return false;
  }

  bool HasPlainTryArg()
  { for(int i=0; i<Args.Length; i++)
      if(Args[i].Name==null && Args[i].Type==ArgType.Normal && Args[i].Expression.ClearsStack) return true;
    return false;
  }

  bool HasTryArg()
  { for(int i=0; i<Args.Length; i++) if(Args[i].Expression.ClearsStack) return true;
    return false;
  }

  static bool FuncNameMatch(Name var, LambdaNode func)
  { Name binding = func.Binding;
    return binding!=null && var.Index==binding.Index && var.String==binding.String &&
           var.Depth==binding.Depth+(func.CreatesLocalEnvironment ? 1 : 0);
  }
}
#endregion

#region DebugNode
public abstract class DebugNode : Node
{ public override Type GetNodeType() { return typeof(void); }
  public override void Walk(IWalker w) { }
}
#endregion

#region DeleteNode
public class DeleteNode : SetNodeBase
{ public DeleteNode(Node[] nodes) { Nodes = nodes; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { cg.MarkPosition(this);
    foreach(Node n in Nodes) EmitDelete(cg, n);
    if(etype!=typeof(void))
    { cg.EmitNull();
      etype = typeof(object);
    }
    TailReturn(cg);
  }

  public override object Evaluate()
  { foreach(Node n in Nodes) Delete(n);
    return null;
  }

  public override MutatedName[] GetMutatedNames()
  { ArrayList names = new ArrayList();
    foreach(Node n in Nodes) GetMutatedNames(names, n);
    return (MutatedName[])names.ToArray(typeof(MutatedName));
  }

  public override Type GetNodeType() { return typeof(void); }

  public override void MarkTail(bool tail)
  { foreach(Node n in Nodes) n.MarkTail(false);
    Tail = tail;
  }

  public override void UpdateNames(MutatedName[] names)
  { int i = 0;
    foreach(Node n in Nodes) UpdateNames(names, ref i, n);
    if(i!=names.Length) throw new InvalidOperationException("UpdateNames: Not all names were consumed");
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this)) foreach(Node n in Nodes) n.Walk(w);
    w.PostWalk(this);
  }

  public readonly Node[] Nodes;

  protected virtual void Delete(Node node)
  { if(node is VariableNode)
    { VariableNode vn = (VariableNode)node;
      InterpreterEnvironment cur = InterpreterEnvironment.Current;
      if(vn.Name.Depth==Name.Global || cur==null) TopLevel.Current.Unbind(vn.Name.String);
      else cur.Set(vn.Name.String, Binding.Unbound);
    }
    else throw UnhandledNodeType(node);
  }

  protected virtual void EmitDelete(CodeGenerator cg, Node node)
  { if(node is VariableNode) cg.EmitDelete(((VariableNode)node).Name);
    else throw UnhandledNodeType(node);
  }

  // TODO: currently, this has to be kept in sync with SetNodeBase.GetMutatedNames(), and the same for UpdateNames(). fix that.
  protected virtual void GetMutatedNames(IList names, Node lhs)
  { if(lhs is VariableNode) names.Add(new MutatedName(((VariableNode)lhs).Name));
    else throw UnhandledNodeType(lhs);
  }

  protected virtual void UpdateNames(MutatedName[] names, ref int i, Node lhs)
  { if(lhs is VariableNode) ((VariableNode)lhs).Name = names[i++].Name;
    else throw UnhandledNodeType(lhs);
  }

  protected static Exception UnhandledNodeType(Node node)
  { return new NotSupportedException("Unable to delete nodes of type "+node.GetType().FullName);
  }
}
#endregion

#region ExceptionNode
public abstract class ExceptionNode : Node
{ public ExceptionNode(Node body, Except[] excepts) { Body=body; Excepts=excepts; }

  public Label LeaveLabel { get { return InTry==null ? leaveLabel : InTry.LeaveLabel; } }
  public Slot  ReturnSlot { get { return InTry==null ? returnSlot : InTry.ReturnSlot; } }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(!ClearsStack)
    { if(!NeedElse) // either NeedElse is true or NeedFinally, but not both
      { if(NeedFinally) EmitFinally(cg);
        EmitConstant(cg, Body.Evaluate(), ref etype);
      }
      else
      { EmitElse(cg);
        EmitConstant(cg, Body.Evaluate(), ref etype);
      }
      return;
    }

    returnSlot = etype==typeof(void) ? null : cg.AllocLocalTemp(typeof(object), Body.Interrupts);
    Debug.Assert(returnSlot!=null || !Tail);
    cg.MarkPosition(this);

    bool haveExcept = Excepts!=null && Excepts.Length!=0;
    Slot fromElse;
    
    if(!NeedElse || !haveExcept) fromElse = null;
    else
    { fromElse = cg.AllocLocalTemp(typeof(bool), Body.Interrupts);
      cg.EmitBool(false);
      fromElse.EmitSet(cg);
    }

    EmitPreTry(cg);
    leaveLabel = cg.ILG.BeginExceptionBlock();

    if(returnSlot==null)
    { Body.Emit(cg, ref etype);
      if(etype!=typeof(void))
      { etype = typeof(void);
        cg.ILG.Emit(OpCodes.Pop);
      }
    }
    else
    { etype = typeof(object);
      Body.Emit(cg);
      if(!Tail) returnSlot.EmitSet(cg);
    }

    if(NeedElse)
    { if(fromElse!=null)
      { cg.EmitBool(true);
        fromElse.EmitSet(cg);
      }
      EmitElse(cg);
    }

    if(haveExcept)
    { cg.ILG.BeginCatchBlock(typeof(Exception));

      Slot eslot=null;
      bool needRethrow=true;
      Label rethrowLabel;

      if(fromElse==null) rethrowLabel = new Label();
      else
      { rethrowLabel = cg.ILG.DefineLabel();
        needRethrow  = true;
        fromElse.EmitGet(cg);
        cg.ILG.Emit(OpCodes.Brtrue, rethrowLabel);
      }

      MethodInfo expectType=typeof(Ops).GetMethod("ExpectType"), isInst=typeof(Type).GetMethod("IsInstanceOfType");
      FieldInfo rtType = typeof(ReflectedType).GetField("Type");

      foreach(Except ex in Excepts)
      { Label next;
        if(ex.Types==null)
        { needRethrow = false;
          next = new Label();
        }
        else
        { Label body = cg.ILG.DefineLabel();
          next = cg.ILG.DefineLabel();
          if(eslot==null)
          { eslot = cg.AllocLocalTemp(typeof(Exception));
            eslot.EmitSet(cg);
          }
          for(int i=0; i<ex.Types.Length; i++)
          { Type ttype = typeof(Type);
            ex.Types[i].Emit(cg, ref ttype);
            if(ttype!=typeof(Type))
            { if(ttype!=typeof(ReflectedType)) cg.EmitCall(expectType);
              cg.EmitFieldGet(rtType);
            }
            eslot.EmitGet(cg);
            cg.EmitCall(isInst);
            if(i<ex.Types.Length-1) cg.ILG.Emit(OpCodes.Brtrue, body);
            else cg.ILG.Emit(OpCodes.Brfalse, next);
          }
          cg.ILG.MarkLabel(body);
        }

        if(ex.Var!=null)
        { if(eslot==null) cg.EmitSet(ex.Var);
          else cg.EmitSet(ex.Var, eslot);
        }

        if(returnSlot==null || ex.Body.GetNodeType()==typeof(void)) ex.Body.EmitVoid(cg);
        else
        { ex.Body.Emit(cg);
          returnSlot.EmitSet(cg);
        }

        cg.ILG.Emit(OpCodes.Leave, Tail ? LeaveLabel : leaveLabel);
        if(ex.Types==null) break;
        else cg.ILG.MarkLabel(next);
      }

      if(needRethrow)
      { if(fromElse!=null) cg.ILG.MarkLabel(rethrowLabel);
        cg.ILG.Emit(OpCodes.Rethrow);
      }
      if(eslot!=null) cg.FreeLocalTemp(eslot);
    }

    if(NeedFinally)
    { cg.ILG.BeginFinallyBlock();
      EmitFinally(cg);
    }
    cg.ILG.EndExceptionBlock();

    if(fromElse!=null && !Body.Interrupts) cg.FreeLocalTemp(fromElse);

    if(returnSlot!=null)
    { returnSlot.EmitGet(cg);
      if(!Body.Interrupts) cg.FreeLocalTemp(returnSlot);
      returnSlot = null;
      TailReturn(cg);
    }
    else if(etype!=typeof(void)) cg.EmitNull();

    leaveLabel = new Label();
  }

  public override object Evaluate()
  { ExecutePreTry();

    if(Excepts==null)
      try { object ret=Body.Evaluate(); ExecuteElse(); return ret; }
      finally { if(NeedFinally) ExecuteFinally(); }
    else
    { bool fromElse=false;

      try { object ret=Body.Evaluate(); fromElse=true; ExecuteElse(); return ret; }
      catch(Exception e)
      { if(!fromElse)
          foreach(Except ex in Excepts)
          { if(ex.Types!=null)
            { bool isMatch=false;  
              foreach(VariableNode name in ex.Types)
              { Type type = Ops.ExpectType(name.Evaluate()).Type;
                if(type.IsInstanceOfType(e)) { isMatch=true; break; }
              }
              if(!isMatch) continue;
            }

            object ret;
            if(ExceptionStack==null) ExceptionStack = new Stack();
            ExceptionStack.Push(e);

            if(ex.Var==null)
              try { ret=ex.Body.Evaluate(); }
              finally { ExceptionStack.Pop(); }
            else
            { InterpreterEnvironment ne, old=InterpreterEnvironment.Current;
              try
              { InterpreterEnvironment.Current = ne = new InterpreterEnvironment(old);
                ne.Bind(ex.Var.String, e);
                ret = ex.Body.Evaluate();
              }
              finally { InterpreterEnvironment.Current=old; ExceptionStack.Pop(); }
            }
            return ret;
          }

        throw;
      }
      finally { if(NeedFinally) ExecuteFinally(); }
    }
  }

  public override Type GetNodeType() { return Body.GetNodeType(); }

  public override void MarkTail(bool tail)
  { Tail = tail;
    Body.MarkTail(tail);
    if(Excepts!=null)
      foreach(Except ex in Excepts)
      { if(ex.Types!=null) foreach(Node n in ex.Types) n.MarkTail(false);
        ex.Body.MarkTail(false);
      }
  }

  public override void SetFlags()
  { ClearsStack = !Body.IsConstant && (!NeedFinally || !NeedElse);
    Interrupts  = Body.Interrupts;
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this)) BaseWalk(w);
    w.PostWalk(this);
  }

  public readonly Node Body;
  public readonly Except[] Excepts;

  [ThreadStatic] public static Stack ExceptionStack;

  protected virtual void EmitPreTry(CodeGenerator cg) { }
  protected virtual void EmitBody(CodeGenerator cg) { Body.Emit(cg); }
  protected virtual void EmitElse(CodeGenerator cg) { }
  protected virtual void EmitFinally(CodeGenerator cg) { }

  protected virtual void ExecutePreTry() { }
  protected virtual void ExecuteElse() { }
  protected virtual void ExecuteFinally() { }

  protected void BaseWalk(IWalker w)
  { Body.Walk(w);
    if(Excepts!=null)
      foreach(Except ex in Excepts)
      { if(ex.Types!=null) foreach(Node n in ex.Types) n.Walk(w);
        ex.Body.Walk(w);
      }
  }

  protected bool NeedElse, NeedFinally;

  Label leaveLabel;
  Slot returnSlot;
}
#endregion

#region IfNode
public sealed class IfNode : Node
{ public IfNode(Node test, Node iftrue) : this(test, iftrue, null) { }
  public IfNode(Node test, Node iftrue, Node iffalse) { Test=test; IfTrue=iftrue; IfFalse=iffalse; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { cg.MarkPosition(this);

    if(IsConstant)
    { EmitConstant(cg, Evaluate(), ref etype);
      TailReturn(cg);
    }
    else if(Test.IsConstant)
    { if(Ops.IsTrue(Test.Evaluate())) IfTrue.Emit(cg, ref etype);
      else if(IfFalse!=null) IfFalse.Emit(cg, ref etype);
      else if(etype!=typeof(void))
      { cg.EmitNull();
        etype = typeof(object);
      }
    }
    else
    { Node ifTrue=IfTrue, ifFalse=IfFalse;

      bool trueJump = false;
      if(Options.Current.OptimizeAny)
      { if(ifTrue is JumpNode && !((JumpNode)ifTrue).NeedsLeave) trueJump = true;
        else if(ifFalse!=null && ifFalse is JumpNode && !((JumpNode)ifFalse).NeedsLeave)
        { Node t=ifTrue; ifTrue=ifFalse; ifFalse=t;
          trueJump = true;
        }
      }

      Type truetype=ifTrue.GetNodeType(), falsetype = ifFalse==null ? null : ifFalse.GetNodeType();
      if(etype==typeof(void)) truetype = falsetype = typeof(void);
      else if((ifFalse==null || truetype==falsetype) && AreCompatible(truetype, etype)) etype = falsetype = truetype;
      else if(truetype==typeof(void) && (ifFalse==null || falsetype==typeof(void)))
      { falsetype = typeof(void);
        etype = typeof(object);
      }
      else truetype = falsetype = etype = typeof(object);

      bool hasEnd = !ifTrue.Tail && (ifFalse!=null || truetype!=typeof(void));
      Label endlbl=hasEnd ? cg.ILG.DefineLabel() : new Label(), falselbl=trueJump ? new Label() : cg.ILG.DefineLabel();

      Type type = typeof(bool);
      Test.Emit(cg, ref type);
      if(trueJump) // a peephole optimization for "if(cond) goto label;" to avoid an ugly IL sequence
      { JumpNode jn = (JumpNode)ifTrue;
        if(type==typeof(bool)) cg.ILG.Emit(OpCodes.Brtrue, jn.Label);
        else if(type==typeof(CodeGenerator.negbool)) cg.ILG.Emit(OpCodes.Brfalse, jn.Label);
        else
        { cg.EmitIsTrue();
          cg.ILG.Emit(OpCodes.Brtrue, jn.Label);
        }
      }
      else
      { if(type==typeof(bool)) cg.ILG.Emit(OpCodes.Brfalse, falselbl);
        else if(type==typeof(CodeGenerator.negbool)) cg.ILG.Emit(OpCodes.Brtrue, falselbl);
        else
        { cg.EmitIsTrue();
          cg.ILG.Emit(OpCodes.Brfalse, falselbl);
        }

        if(truetype==typeof(void)) ifTrue.EmitVoid(cg);
        else
        { ifTrue.Emit(cg, ref truetype);
          Debug.Assert(AreCompatible(truetype, etype));
        }
      }

      if(hasEnd) cg.ILG.Emit(OpCodes.Br, endlbl);
      if(!trueJump) cg.ILG.MarkLabel(falselbl);

      if(ifFalse!=null || truetype!=typeof(void))
      { if(falsetype==typeof(void))
        { if(ifFalse!=null) ifFalse.EmitVoid(cg);
        }
        else
        { cg.EmitNode(ifFalse, ref falsetype);
          Debug.Assert(AreCompatible(falsetype, etype));
          if(ifFalse==null && ifTrue.Tail) TailReturn(cg);
        }
        if(hasEnd) cg.ILG.MarkLabel(endlbl);
      }
      if(truetype==typeof(void) && etype!=typeof(void)) cg.EmitNull();
      if(!ifTrue.Tail) TailReturn(cg);
    }
  }

  public override object Evaluate()
  { if(Ops.IsTrue(Test.Evaluate())) return IfTrue.Evaluate();
    else if(IfFalse!=null) return IfFalse.Evaluate();
    else return null;
  }

  public override Type GetNodeType()
  { if(IsConstant)
      return Ops.IsTrue(Test.Evaluate()) ? IfTrue.GetNodeType() : IfFalse!=null ? IfFalse.GetNodeType() : null;
    Type truetype=IfTrue.GetNodeType(), falsetype=IfFalse==null ? null : IfFalse.GetNodeType();
    return truetype==falsetype ? truetype : typeof(object);
  }

  public override void MarkTail(bool tail)
  { Tail = tail;
    Test.MarkTail(false);
    if(tail && IfTrue.GetNodeType()==typeof(void) && (IfFalse==null || IfFalse.GetNodeType()==typeof(void)))
      tail = false;
    IfTrue.MarkTail(tail);
    if(IfFalse!=null) IfFalse.MarkTail(tail);
  }

  public override void Optimize()
  { if(Test.IsConstant)
    { bool test = Ops.IsTrue(Test.Evaluate());
      IsConstant = test && IfTrue.IsConstant || !test && (IfFalse==null || IfFalse.IsConstant);
    }
  }

  public override void SetFlags()
  { if(Test.IsConstant)
    { Node path = Ops.IsTrue(Test.Evaluate()) ? IfTrue : IfFalse;
      if(path!=null)
      { ClearsStack = path.ClearsStack;
        Interrupts  = path.Interrupts;
      }
    }
    else
    { ClearsStack = HasTryNode(Test, IfTrue, IfFalse);
      Interrupts  = HasInterrupt(Test, IfTrue, IfFalse);
    }
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this))
    { Test.Walk(w);
      IfTrue.Walk(w);
      if(IfFalse!=null) IfFalse.Walk(w);
    }
    w.PostWalk(this);
  }

  public readonly Node Test, IfTrue, IfFalse;
}
#endregion

#region JumpNode
public abstract class JumpNode : Node
{ public JumpNode() { }
  public JumpNode(string name) { Name=name; }
  public JumpNode(Label label) { Label=label; }
  public JumpNode(Label label, bool needsLeave) { Label=label; NeedsLeave=needsLeave; }

  // note that IfNode makes assumptions about this
  public override void Emit(CodeGenerator cg, ref Type etype)
  { cg.ILG.Emit(NeedsLeave ? OpCodes.Leave : OpCodes.Br, Label);
    etype = typeof(void);
  }

  public override Type GetNodeType() { return typeof(void); }

  public Label Label;
  public string Name;
  public bool   NeedsLeave;
}
#endregion

#region LambdaNode
public sealed class LambdaNode : Node
{ public LambdaNode(Node body) : this(null, new Parameter[0], body) { }
  public LambdaNode(string[] parms, Node body) : this(null, parms, body) { }
  public LambdaNode(string name, string[] parms, Node body) : this(name, StringsToParams(parms, false, false), body) { }
  public LambdaNode(string[] parms, bool hasList, bool hasDict, Node body)
    : this(null, StringsToParams(parms, hasList, hasDict), body) { }
  public LambdaNode(string name, string[] parms, bool hasList, bool hasDict, Node body)
    : this(name, StringsToParams(parms, hasList, hasDict), body) { }
  public LambdaNode(Parameter[] parms, Node body) : this(null, parms, body) { }
  public LambdaNode(string name, Parameter[] parms, Node body)
  { Parameter.CheckParms(parms, out NumRequired, out OptionalStart, out NumOptional, out HasList, out HasDict);
    Name=name; Parameters=parms; Body=body;

    for(int i=0; i<parms.Length; i++)
    { parms[i].Name.Index = i;
      parms[i].Name.Depth = 0;
    }
  }

  public bool CreatesLocalEnvironment { get { return ClosedVars!=0 || ClosedParams!=0; } }

  public int CloseParameter(int i)
  { Parameters[i].Closed = true;
    Parameters[i].Name.Depth = 1;
    return Parameters[i].Name.Index = ClosedParams++;
  }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { cg.MarkPosition(this);
    if(etype!=typeof(void))
    { index = lindex.Next;
      CodeGenerator impl = MakeImplMethod(cg);

      Slot tmpl;
      if(!cg.TypeGenerator.GetNamedConstant("template"+index, typeof(Template), out tmpl))
      { CodeGenerator icg = cg.TypeGenerator.GetInitializer();
        icg.ILG.Emit(OpCodes.Ldftn, (MethodInfo)impl.MethodBase);
        icg.EmitLanguage(Ops.GetCurrentLanguage());
        icg.EmitString(Name!=null ? Name : Binding!=null ? Binding.String : null);
        icg.EmitConstantObject(Parameter.GetNames(Parameters));
        icg.EmitInt(NumRequired);
        icg.EmitBool(HasList);
        icg.EmitBool(HasDict);
        icg.EmitBool(ClosedParams!=0 && ClosedVars==0);
        icg.EmitNew(typeof(Template), typeof(IntPtr), typeof(Language), typeof(string), typeof(string[]), typeof(int),
                    typeof(bool), typeof(bool), typeof(bool));
        tmpl.EmitSet(icg);
      }

      tmpl.EmitGet(cg);
      cg.EmitArgGet(0);
      if(NumOptional==0) cg.EmitNew(RG.ClosureType, typeof(Template), typeof(LocalEnvironment));
      else
      { EmitDefaults(cg);
        cg.EmitNew(RG.ClosureType, typeof(Template), typeof(LocalEnvironment), typeof(object[]));
      }
      etype = RG.ClosureType;
    }
    TailReturn(cg);
  }

  public override object Evaluate()
  { return new InterpretedProcedure(Name!=null ? Name : Binding!=null ? Binding.String : null, Parameters, Body);
  }

  public override Type GetNodeType() { return RG.ClosureType; }

  public override void MarkTail(bool tail)
  { Tail = tail;
    Body.MarkTail(true);
  }

  public override void SetFlags()
  { bool clears=false, interrupts=false;
    for(int i=0; i<NumOptional; i++)
    { Node def = Parameters[i+OptionalStart].Default;
      if(def.ClearsStack) clears = true;
      if(def.Interrupts) interrupts = true;
    }
    ClearsStack = clears;
    Interrupts  = interrupts;
  }

  public override void Walk(IWalker w) // this has to be kept in sync with NodeDecorator.Walk(), unfortunately
  { if(w.Walk(this)) Body.Walk(w);
    w.PostWalk(this);
  }

  public readonly Parameter[] Parameters;
  public Node Body;
  public Name Binding;
  public string Name;
  public Label StartLabel;
  public int ClosedParams, ClosedVars;
  public readonly int NumRequired, NumOptional, OptionalStart;
  public readonly bool HasList, HasDict;

  void EmitDefaults(CodeGenerator cg)
  { if(NumOptional==0) { cg.EmitNull(); return; }

    bool constant = true;
    for(int i=0; i<NumOptional; i++) if(!Parameters[i+OptionalStart].Default.IsConstant) { constant=false; break; }

    if(constant)
    { object[] objs = new object[NumOptional];
      for(int i=0; i<NumOptional; i++) objs[i] = Parameters[i+OptionalStart].Default.Evaluate();
      cg.EmitConstantObject(objs);
    }
    else
    { Node[] nodes = new Node[NumOptional];
      for(int i=0; i<NumOptional; i++) nodes[i] = Parameters[i+OptionalStart].Default;
      cg.EmitObjectArray(nodes);
    }
  }

  CodeGenerator MakeImplMethod(CodeGenerator cg)
  { CodeGenerator icg;
    string name = "lambda$"+index.ToString();
    if(Name!=null) name += "_"+Name;
    icg = cg.TypeGenerator.DefineStaticMethod(name, typeof(object), typeof(LocalEnvironment), typeof(object[]));

    icg.Namespace = new LocalNamespace(cg.Namespace, icg);
    if(CreatesLocalEnvironment)
    { icg.EmitArgGet(0);
      if(ClosedVars==0)
      { icg.EmitArgGet(1);
        icg.EmitNew(typeof(LocalEnvironment), typeof(LocalEnvironment), typeof(object[]));
      }
      else if(ClosedParams==0)
      { icg.EmitInt(ClosedVars);
        icg.EmitNew(typeof(LocalEnvironment), typeof(LocalEnvironment), typeof(int));
      }
      else if(ClosedParams==Parameters.Length)
      { icg.EmitInt(ClosedParams+ClosedVars);
        icg.EmitArgGet(1);
        icg.EmitNew(typeof(LocalEnvironment), typeof(LocalEnvironment), typeof(int), typeof(object[]));
      }
      else
      { icg.EmitInt(ClosedParams+ClosedVars);
        icg.EmitNew(typeof(LocalEnvironment), typeof(LocalEnvironment), typeof(int));
        icg.Dup();
        icg.EmitFieldGet(typeof(LocalEnvironment), "Values");
        for(int i=0,j=0; i<Parameters.Length; i++)
          if(Parameters[i].Closed)
          { if(j!=ClosedParams-1) icg.Dup();
            icg.EmitInt(j++);
            icg.EmitArgGet(1);
            icg.EmitInt(i);
            icg.ILG.Emit(OpCodes.Ldelem_Ref);
            icg.ILG.Emit(OpCodes.Stelem_Ref);
          }
      }
      icg.EmitArgSet(0);
    }

    StartLabel = icg.ILG.DefineLabel();
    icg.ILG.MarkLabel(StartLabel);
    Body.Emit(icg);
    icg.Finish();
    return icg;
  }

  long index;

  static Parameter[] StringsToParams(string[] names, bool hasList, bool hasDict)
  { Parameter[] parms = new Parameter[names.Length];
    for(int i=0; i<names.Length; i++) parms[i] = new Parameter(names[i]);
    if(hasDict) parms[names.Length-1].Type = ParamType.Dict;
    if(hasList) parms[names.Length-(hasDict ? 2 : 1)].Type = ParamType.List;
    return parms;
  }

  static Index lindex = new Index();
}
#endregion

#region LastNode
public sealed class LastNode : Node
{ public override void Emit(Scripting.CodeGenerator cg, ref Type etype)
  { if(etype!=typeof(void))
    { cg.EmitFieldGet(typeof(Ops), "LastPtr");
      etype = typeof(object);
      TailReturn(cg);
    }
  }

  public override object Evaluate() { return Ops.LastPtr; }
  public override Type GetNodeType() { return typeof(object); }
  public override void Walk(IWalker w) { w.Walk(this); w.PostWalk(this); }
}
#endregion

#region LiteralNode
public sealed class LiteralNode : Node
{ public LiteralNode(object value) { Value=value; }
  public override void Emit(CodeGenerator cg, ref Type etype)
  { EmitConstant(cg, Value, ref etype);
    TailReturn(cg);
  }
  public override object Evaluate() { return Value; }
  public override Type GetNodeType() { return Value==null ? null : Value.GetType(); }
  public override void Optimize() { IsConstant = true; }

  public readonly object Value;
}
#endregion

#region LocalBindNode
public sealed class LocalBindNode : Node
{ public LocalBindNode(string name, Node init, Node body) : this(new string[] { name }, new Node[] { init }, null, body) { }
  public LocalBindNode(string name, Node init, Type type, Node body)
    : this(new string[] { name }, new Node[] { init }, new Type[] { type }, body) { }
  public LocalBindNode(string[] names, Node[] inits, Node body) : this(names, inits, null, body) { }
  public LocalBindNode(string[] names, Node[] inits, Type[] types, Node body)
  { Inits=inits; Body=body;

    Names = new Name[names.Length];
    for(int i=0; i<names.Length; i++) Names[i] = new Name(names[i], types==null ? typeof(object) : types[i]);
  }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { cg.MarkPosition(this);
    for(int i=0; i<Inits.Length; i++)
      if(Inits[i]!=null) cg.EmitSet(Names[i], Inits[i]);
      else if(Options.Current.Debug)
      { cg.EmitFieldGet(typeof(Binding), "Unbound");
        cg.EmitSet(Names[i]);
      }
    Body.Emit(cg, ref etype);
    for(int i=0; i<Names.Length; i++) cg.Namespace.RemoveSlot(Names[i]);
  }

  public override object Evaluate()
  { if(IsConstant || Inits.Length==0) return Body.Evaluate();

    InterpreterEnvironment ne, old=InterpreterEnvironment.Current;
    try
    { InterpreterEnvironment.Current = ne = new InterpreterEnvironment(old);
      for(int i=0; i<Inits.Length; i++)
        ne.Bind(Names[i].String, Inits[i]==null ? null : Inits[i].Evaluate());
      return Body.Evaluate();
    }
    finally { InterpreterEnvironment.Current=old; }
  }

  public override Type GetNodeType() { return Body.GetNodeType(); }

  public override void MarkTail(bool tail)
  { foreach(Node n in Inits) if(n!=null) n.MarkTail(false);
    Body.MarkTail(tail);
  }

  public override void Optimize()
  { bool isconst = Body.IsConstant;
    if(isconst) foreach(Node n in Inits) if(n!=null && !n.IsConstant) { isconst=false; break; }
    IsConstant = isconst;
  }

  public override void SetFlags()
  { ClearsStack = Body.ClearsStack || HasTryNode(Inits);
    Interrupts  = Body.Interrupts  || HasInterrupt(Inits);
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this))
    { foreach(Node n in Inits) if(n!=null) n.Walk(w);
      Body.Walk(w);
    }
    w.PostWalk(this);
  }

  public readonly Name[] Names;
  public readonly Node[] Inits;
  public readonly Node Body;
}
#endregion

#region MarkerNode
public abstract class MarkerNode : Node
{ public override void Emit(CodeGenerator cg, ref Type etype)
  { if(etype!=typeof(void)) { cg.EmitNull(); etype=typeof(object); }
    TailReturn(cg);
  }

  public override object Evaluate() { return null; }
  public override Type GetNodeType() { return null; }
}
#endregion

#region MarkSourceNode
public sealed class MarkSourceNode : DebugNode
{ public MarkSourceNode(string file, string code, Node body) { File=file; Code=code; Body=body; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(Options.Current.Debug && cg.TypeGenerator.Assembly.IsDebug)
      cg.TypeGenerator.Assembly.Symbols =
        cg.TypeGenerator.Assembly.Module.DefineDocument(File, Guid.Empty, Guid.Empty, Guid.Empty);
    // TODO: figure this out. cg.TypeGenerator.Assembly.Symbols.SetSource(System.Text.Encoding.UTF8.GetBytes(Code));
    if(Body!=null) Body.Emit(cg, ref etype);
    else if(etype!=typeof(void))
    { cg.EmitNull();
      etype = typeof(object);
    }
  }

  public override object Evaluate() { return Body==null ? null : Body.Evaluate(); }
  public override Type GetNodeType() { return Body==null ? typeof(object) : Body.GetNodeType(); }
  public override void MarkTail(bool tail) { Tail = tail; if(Body!=null) Body.MarkTail(tail); }
  public override void Walk(IWalker w)
  { if(w.Walk(this) && Body!=null) Body.Walk(w);
    w.PostWalk(this);
  }

  public readonly string File, Code;
  public readonly Node Body;
}
#endregion

// dotted names are only supported if 'members' is a constant value (members.IsConstant is true). otherwise, 'members'
// must evaluate to a non-dotted string
#region MemberNode
public class MemberNode : Node
{ public MemberNode(Node value, Node members) : this(value, members, true) { }
  public MemberNode(Node value, Node members, bool enableCache)
  { Value=value; Members=members; EnableCache=enableCache;
  }

  public sealed class CachePromise
  { public Slot GetCache(CodeGenerator cg)
    { if(cache==null)
        cache = cg.TypeGenerator.DefineStaticField("tc$"+cindex.Next, typeof(MemberCache));
      return cache;
    }

    Slot cache;

    static Index cindex = new Index();
  }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { Value.Emit(cg);
    if(Members.IsConstant)
    { string[] bits;
      { string str = Members.Evaluate() as string;
        if(str==null) throw Ops.SyntaxError(this, "MemberNode expects a string value as the second argument");
        bits = str.Split('.');
      }

      Label done;
      Slot cache;
      if(!EnableCache || !Options.Current.OptimizeSpeed) { cache=null; done=new Label(); }
      else
      { Label miss=cg.ILG.DefineLabel(), isNull=cg.ILG.DefineLabel();
        cache = (Cache==null || Options.Current.IsPreCompilation ? new CachePromise() : Cache).GetCache(cg);
        done  = cg.ILG.DefineLabel();

        cg.Dup();
        cg.ILG.Emit(OpCodes.Brfalse_S, isNull);
        cg.Dup();
        Slot tmp = cg.AllocLocalTemp(typeof(object));
        tmp.EmitSet(cg);
        cg.EmitCall(typeof(MemberCache), "TypeFromObject"); // TODO: maybe we should inline this
        cache.EmitGetAddr(cg);
        cg.EmitFieldGet(typeof(MemberCache), "Type");
        cg.ILG.Emit(OpCodes.Bne_Un_S, miss);
        HandleThisPtr(cg, tmp);
        cache.EmitGetAddr(cg);
        cg.EmitFieldGet(typeof(MemberCache), "Value");
        cg.ILG.Emit(OpCodes.Br, done);
        cg.ILG.MarkLabel(miss);
        cache.EmitGetAddr(cg);
        tmp.EmitGet(cg);
        cg.EmitCall(typeof(MemberCache), "TypeFromObject"); // TODO: maybe we should inline this
        cg.EmitFieldSet(typeof(MemberCache), "Type");
        tmp.EmitGet(cg);
        cg.ILG.MarkLabel(isNull);

        cg.FreeLocalTemp(tmp);
      }

      for(int i=0; i<bits.Length; i++)
      { if(i==bits.Length-1) HandleThisPtr(cg);
        cg.EmitCall(typeof(MemberContainer), "FromObject");
        cg.EmitString(bits[i]);
        cg.EmitCall(typeof(MemberContainer), "GetSlot", typeof(string));
      }

      if(cache!=null)
      { Slot tmp = cg.AllocLocalTemp(typeof(object));
        tmp.EmitSet(cg);
        cache.EmitGetAddr(cg);
        tmp.EmitGet(cg);
        cg.EmitFieldSet(typeof(MemberCache), "Value");
        tmp.EmitGet(cg);
        cg.ILG.MarkLabel(done);
        cg.FreeLocalTemp(tmp);
      }
    }
    else
    { Slot tmp1, tmp2;
      HandleThisPtr(cg);
      if(Members is TryNode)
      { tmp1 = cg.AllocLocalTemp(typeof(object), Members.Interrupts);
        tmp1.EmitSet(cg);
      }
      else { tmp1 = tmp2 = null; }
      Members.EmitTyped(cg, typeof(string));
      if(tmp1!=null)
      { tmp2 = cg.AllocLocalTemp(typeof(string));
        tmp2.EmitSet(cg);
        tmp1.EmitGet(cg);
        tmp2.EmitGet(cg);

        if(!Members.Interrupts) cg.FreeLocalTemp(tmp1);
        cg.FreeLocalTemp(tmp2);
      }
      cg.EmitCall(typeof(Ops), "GetSlot", typeof(object), typeof(string));
    }
    etype = typeof(object);
    TailReturn(cg);
  }

  public override object Evaluate()
  { object value = Value.Evaluate();
    string str = Ops.ExpectString(Members.Evaluate());
    if(!Members.IsConstant) value = Ops.GetSlot(value, str);
    else foreach(string bit in str.Split('.')) value = Ops.GetSlot(value, bit);
    return value;
  }

  public override Type GetNodeType() { return typeof(object); }

  public override void SetFlags()
  { ClearsStack = Value.ClearsStack || Members.ClearsStack;
    Interrupts  = Value.Interrupts  || Members.Interrupts;
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this))
    { Value.Walk(w);
      Members.Walk(w);
    }
    w.PostWalk(this);
  }

  public readonly Node Value, Members;
  public CachePromise Cache;
  public readonly bool EnableCache;
  
  protected virtual void HandleThisPtr(CodeGenerator cg) { } // the 'this' pointer is on the stack but must be preserved
  protected virtual void HandleThisPtr(CodeGenerator cg, Slot slot) { } // the 'this' pointer is in the slot
}
#endregion

#region OpNode
public sealed class OpNode : Node
{ public OpNode(Operator op, params Node[] nodes) { Operator=op; Nodes=nodes; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(IsConstant) EmitConstant(cg, Evaluate(), ref etype);
    else Operator.Emit(cg, ref etype, Nodes);
    TailReturn(cg);
  }

  public override object Evaluate() { return Operator.Evaluate(Nodes); }
  public override Type GetNodeType() { return Operator.GetResultType(); }

  public override void MarkTail(bool tail)
  { foreach(Node n in Nodes) n.MarkTail(false);
    Tail = tail;
  }

  public override void Optimize() { IsConstant = AreConstant(Nodes); }

  public override void SetFlags()
  { ClearsStack = HasTryNode(Nodes);
    Interrupts  = HasInterrupt(Nodes);
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this)) foreach(Node n in Nodes) n.Walk(w);
    w.PostWalk(this);
  }

  public readonly Operator Operator;
  public readonly Node[] Nodes;
}
#endregion

#region PropMemberNode
public sealed class PropMemberNode : MemberNode
{ public PropMemberNode(Node value, Node member) : base(value, member) { }
  public PropMemberNode(Node value, Node member, bool enableCache) : base(value, member, enableCache) { }

  protected override void HandleThisPtr(Scripting.CodeGenerator cg)
  { cg.Dup();
    cg.EmitFieldSet(typeof(Ops), "LastPtr");
  }

  protected override void HandleThisPtr(Scripting.CodeGenerator cg, Slot slot)
  { slot.EmitGet(cg);
    cg.EmitFieldSet(typeof(Ops), "LastPtr");
  }
}
#endregion

#region PropertyNode
public sealed class PropertyNode : WrapperNode
{ public PropertyNode(Node value, Node member) : this(value, member, (Argument[])null, true) { }
  public PropertyNode(Node value, Node member, Node[] extraArgs)
    : this(value, member, CallNode.NodesToArgs(extraArgs), true) { }
  public PropertyNode(Node value, Node member, Argument[] extraArgs) : this(value, member, extraArgs, true) { }
  public PropertyNode(Node value, Node member, bool enableCache)
    : this(value, member, (Argument[])null, enableCache) { }
  public PropertyNode(Node value, Node member, Node[] extraArgs, bool enableCache)
    : this(value, member, CallNode.NodesToArgs(extraArgs), enableCache) { }
  public PropertyNode(Node value, Node member, Argument[] extraArgs, bool enableCache)
  { Argument[] args;
    if(extraArgs==null) args = new Argument[] { new Argument(new LastNode()) };
    else
    { args = new Argument[extraArgs.Length+1];
      args[0] = new Argument(new LastNode());
      extraArgs.CopyTo(args, 1);
    }

    Node = new CallNode(new PropMemberNode(value, member, enableCache), args);
  }
}
#endregion

#region RestartNode
public sealed class RestartNode : JumpNode
{ public RestartNode(string name) : base(name) { }
  public override object Evaluate() { throw new RestartException(Name); }
}
#endregion

#region SetNode
public enum SetType { Alter, Bind, Set }

public class SetNode : SetNodeBase
{ public SetNode(string variable, Node rhs) : this(new VariableNode(variable), rhs) { }
  public SetNode(string variable, Node rhs, SetType type) : this(new VariableNode(variable), rhs, type) { }
  public SetNode(Node lhs, Node rhs) : this(new Node[] { lhs }, rhs) { }
  public SetNode(Node lhs, Node rhs, SetType type) : this(new Node[] { lhs }, rhs, type) { }
  public SetNode(Node[] lhs, Node rhs) : this(lhs, rhs, SetType.Alter) { }
  public SetNode(Node[] lhs, Node rhs, SetType type) { LHS=lhs; RHS=rhs; Type=type; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { cg.MarkPosition(this);

    Type type = GetNodeType();
    RHS.Emit(cg, ref type);
    for(int i=LHS.Length-1; i>=0; i--)
    { if(i!=0 || etype!=typeof(void)) cg.Dup();
      EmitSet(cg, LHS[i], type);
    }
    if(etype!=typeof(void)) EmitTryConvert(cg, type, ref etype);
    TailReturn(cg);
  }

  public override object Evaluate()
  { object value = RHS.Evaluate();
    for(int i=LHS.Length-1; i>=0; i--) Assign(LHS[i], value);
    return value;
  }

  public override MutatedName[] GetMutatedNames()
  { ArrayList names = new ArrayList();
    foreach(Node n in LHS) GetMutatedNames(names, n);
    return (MutatedName[])names.ToArray(typeof(MutatedName));
  }

  public override Type GetNodeType()
  { Type type = LHS[0].GetNodeType();
    for(int i=1; i<LHS.Length; i++) if(LHS[i].GetNodeType()!=type) { type=typeof(object); break; }
    return type;
  }

  public override void MarkTail(bool tail)
  { foreach(Node node in LHS) node.MarkTail(false);
    RHS.MarkTail(false);
    Tail = tail;
  }

  public override void SetFlags()
  { ClearsStack = RHS.ClearsStack;
    Interrupts  = RHS.Interrupts;
  }

  public override void UpdateNames(MutatedName[] names)
  { int i = 0;
    foreach(Node n in LHS) UpdateNames(names, ref i, n);
    if(i!=names.Length) throw new InvalidOperationException("UpdateNames: Not all names were consumed");
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this))
    { foreach(Node n in LHS) n.Walk(w);
      RHS.Walk(w);
    }
    w.PostWalk(this);
  }

  public readonly Node[] LHS;
  public readonly Node RHS;
  public readonly SetType Type;

  protected virtual void Assign(Node lhs, object value)
  { if(lhs is VariableNode)
    { VariableNode vn = (VariableNode)lhs;
      InterpreterEnvironment cur = InterpreterEnvironment.Current;
      if(vn.Name.Depth==Name.Global || cur==null)
      { switch(Type)
        { case SetType.Alter: TopLevel.Current.Alter(vn.Name.String, value); break;
          case SetType.Bind: TopLevel.Current.Bind(vn.Name.String, value); break;
          case SetType.Set: TopLevel.Current.Set(vn.Name.String, value); break;
        }
      }
      else cur.Set(vn.Name.String, value);
    }
    else throw UnhandledNodeType(lhs);
  }

  protected virtual void EmitSet(CodeGenerator cg, Node lhs, Type onStack)
  { if(lhs is VariableNode)
    { VariableNode vn = (VariableNode)lhs;
      if(Type==SetType.Alter || vn.Name.Depth!=Name.Global)
      { EmitStrictConvert(cg, onStack, vn.GetNodeType()); // TODO: why not use cg.EmitConvertTo() ?
        cg.EmitSet(vn.Name);
      }
      else
      { Slot tmp = cg.AllocLocalTemp(typeof(object));
        tmp.EmitSet(cg);
        cg.EmitTopLevel();
        cg.EmitString(vn.Name.String);
        tmp.EmitGet(cg);
        cg.EmitCall(typeof(TopLevel), Type==SetType.Set ? "Set" : "Bind", typeof(string), typeof(object));
        cg.FreeLocalTemp(tmp);
      }
    }
    else throw UnhandledNodeType(lhs);
  }

  protected virtual void GetMutatedNames(IList names, Node lhs)
  { if(lhs is VariableNode) names.Add(new MutatedName(((VariableNode)lhs).Name, RHS));
    else throw UnhandledNodeType(lhs);
  }

  protected virtual void UpdateNames(MutatedName[] names, ref int i, Node lhs)
  { if(lhs is VariableNode) ((VariableNode)lhs).Name = names[i++].Name;
    else throw UnhandledNodeType(lhs);
  }

  protected Exception UnhandledNodeType(Node node)
  { return new NotSupportedException("Unable to assign to nodes of type "+node.GetType().FullName);
  }
}
#endregion

#region SetNodeBase
public abstract class SetNodeBase : Node
{ public abstract MutatedName[] GetMutatedNames();
  public abstract void UpdateNames(MutatedName[] names);
}
#endregion

#region SlotNode
public sealed class SlotNode : Node
{ public SlotNode(Slot slot) { Slot=slot; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { Slot.EmitGet(cg);

    if(AreCompatible(Slot.Type, etype)) etype = Slot.Type;
    else if(!etype.IsValueType && Slot.Type.IsValueType)
    { cg.ILG.Emit(OpCodes.Box, Slot.Type);
      etype = typeof(object);
    }
    else throw new InvalidOperationException("incompatible types");
    TailReturn(cg);
  }
  
  public override object Evaluate() { throw new InvalidOperationException("SlotNodes cannot be evaluated"); }
  public override Type GetNodeType() { return Slot.Type; }

  public readonly Slot Slot;
}
#endregion

#region ThrowNode
public sealed class ThrowNode : Node
{ public ThrowNode() { }
  public ThrowNode(Node exception) { Exception = exception; }
  public ThrowNode(Node exception, params Node[] objects) { Exception=exception; Objects=objects; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { cg.MarkPosition(this);
    if(Exception==null) cg.ILG.Emit(OpCodes.Rethrow);
    else
    { Exception.Emit(cg);
      if(Objects==null || Objects.Length==0) cg.EmitNull();
      else if(!HasTryNode(Objects)) cg.EmitObjectArray(Objects);
      else
      { bool keepAround = HasInterrupt(Objects);
        Slot tmp = cg.AllocLocalTemp(typeof(object), keepAround);
        tmp.EmitSet(cg);
        Slot arr = cg.AllocObjectArray(Objects);
        tmp.EmitGet(cg);
        arr.EmitGet(cg);

        if(!keepAround)
        { cg.FreeLocalTemp(tmp);
          cg.FreeLocalTemp(arr);
        }
      }

      cg.EmitCall(typeof(Ops), "MakeException");
      cg.ILG.Emit(OpCodes.Throw);
      if(etype!=typeof(void))
      { cg.EmitNull();
        etype = typeof(object);
      }
    }
  }

  public override Type GetNodeType() { return typeof(void); }

  public override object Evaluate()
  { if(Exception==null) throw (Exception)ExceptionNode.ExceptionStack.Peek();
    else throw Ops.MakeException(Exception.Evaluate(), Objects==null ? null : MakeObjectArray(Objects));
  }

  public override void MarkTail(bool tail)
  { Tail = false;
    if(Exception!=null)
    { Exception.MarkTail(false);
      if(Objects!=null) foreach(Node n in Objects) n.MarkTail(false);
    }
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this))
    { if(Exception!=null)
      { Exception.Walk(w);
        if(Objects!=null) foreach(Node n in Objects) n.Walk(w);
      }
    }
    w.PostWalk(this);
  }

  public readonly Node Exception;
  public readonly Node[] Objects;
}
#endregion

#region TypeNode
public sealed class TypeNode : Node
{ public TypeNode(Type type) { Type=type; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { cg.EmitTypeOf(Type);
    if(etype==typeof(Type)) return;
    else if(etype==typeof(ReflectedType)) cg.EmitCall(typeof(ReflectedType), "FromType");
    else etype = typeof(ReflectedType);
    TailReturn(cg);
  }

  public override object Evaluate() { return Type; }
  public override Type GetNodeType() { return typeof(Type); }
  public override void Optimize() { IsConstant = true; }

  public readonly Type Type;
}
#endregion

#region TryNode
public sealed class TryNode : ExceptionNode
{ public TryNode(Node body, params Except[] excepts) : this(body, null, null, excepts) { }
  public TryNode(Node body, Node final) : this(body, final, null, null) { }
  public TryNode(Node body, Node final, params Except[] excepts) : this(body, final, null, excepts) { }
  public TryNode(Node body, Node final, Node elseNode, params Except[] excepts) : base(body, excepts)
  { Finally  = final;
    Else     = elseNode;
    NeedElse = elseNode!=null;
    NeedFinally = final!=null;
  }

  public override void MarkTail(bool tail)
  { base.MarkTail(tail);
    if(Finally!=null) Finally.MarkTail(false);
    if(Else!=null) Else.MarkTail(false);
  }

  public override void Optimize() { IsConstant = AreConstant(Body, Else, Finally); }

  public override void Walk(IWalker w) // this has to be kept in sync with NodeDecorator.Walk(), unfortunately
  { if(w.Walk(this))
    { BaseWalk(w);
      if(Finally!=null) Finally.Walk(w);
      if(Else!=null) Else.Walk(w);
    }
    w.PostWalk(this);
  }

  public readonly Node Else, Finally;
  
  protected override void EmitElse(CodeGenerator cg) { Else.EmitVoid(cg); }
  protected override void EmitFinally(CodeGenerator cg) { Finally.EmitVoid(cg); }

  protected override void ExecuteElse() { Else.Evaluate(); }
  protected override void ExecuteFinally() { Finally.Evaluate(); }
}
#endregion

#region UnaryOpNode
public sealed class UnaryOpNode : Node
{ public UnaryOpNode(UnaryOperator op, Node value) { Operator=op; Value=value; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(IsConstant) EmitConstant(cg, Evaluate(), ref etype);
    else Operator.Emit(cg, ref etype, Value);
    TailReturn(cg);
  }

  public override object Evaluate() { return Operator.Evaluate(Value.Evaluate()); }
  public override Type GetNodeType() { return Operator.GetResultType(); }

  public override void MarkTail(bool tail)
  { Tail = tail;
    Value.MarkTail(false);
  }

  public override void Optimize() { IsConstant = Value.IsConstant; }

  public override void SetFlags()
  { ClearsStack = Value.ClearsStack;
    Interrupts  = Value.Interrupts;
  }
  
  public override void Walk(IWalker w)
  { if(w.Walk(this)) Value.Walk(w);
    w.PostWalk(this);
  }

  public readonly UnaryOperator Operator;
  public readonly Node Value;
}
#endregion

#region ValueBindNode
public sealed class ValueBindNode : Node
{ public ValueBindNode(Name[][] names, Node[] inits, Node body) { Names=names; Inits=inits; Body=body; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(IsConstant) EmitConstant(cg, Evaluate(), ref etype);
    else
    { cg.MarkPosition(this);
      for(int i=0; i<Names.Length; i++)
      { Name[] bindings = Names[i];
        Label end = new Label();
        bool useEnd = false;

        object constValue = this; // this means "not set"
        { int constLength;
          if(Inits[i].IsConstant)
          { constValue = Inits[i].Evaluate();
            MultipleValues mv = constValue as MultipleValues;
            if(mv==null) constLength = -1;
            else { constLength=mv.Values.Length; constValue=mv.Values; }
          }
          else if(Options.Current.OptimizeAny && Inits[i] is ValuesNode)
          { ValuesNode vn = (ValuesNode)Inits[i];
            constLength = vn.Values.Length;
            if(constLength<bindings.Length) goto checkLength;
            object[] values = new object[bindings.Length];
            for(int j=0; j<bindings.Length; j++)
              values[j] = vn.Values[j].IsConstant ? vn.Values[j].Evaluate() : vn.Values[j];
            constValue = values;
          }
          else goto skip;
          checkLength:
          if(constLength!=-2 && (constLength==-1 ? 1 : constLength) < bindings.Length)
            throw Ops.SyntaxError("expected at least "+bindings.Length.ToString()+" values, but received "+
                                  constLength.ToString());
        }

        skip:
        if(constValue==this)
        { if(bindings.Length!=1)
          { Inits[i].EmitTyped(cg, typeof(MultipleValues));
            cg.EmitInt(bindings.Length);
            cg.EmitCall(typeof(Ops), "CheckValues");
          }
          else
          { Type itype = typeof(MultipleValues);
            Inits[i].Emit(cg, ref itype);
            if(itype==typeof(MultipleValues))
            { cg.EmitInt(bindings.Length);
              cg.EmitCall(typeof(Ops), "CheckValues");
            }
            else
            { Label loop = cg.ILG.DefineLabel();
              end = cg.ILG.DefineLabel();
              useEnd = true;

              cg.Dup();
              Slot tmp = cg.AllocLocalTemp(typeof(object));
              tmp.EmitSet(cg);
              cg.ILG.Emit(OpCodes.Isinst, typeof(MultipleValues));
              cg.Dup();
              cg.ILG.Emit(OpCodes.Brtrue_S, loop);
              cg.ILG.Emit(OpCodes.Pop);
              cg.EmitSet(bindings[0], tmp);
              cg.ILG.Emit(OpCodes.Br, end);

              cg.FreeLocalTemp(tmp);
              cg.ILG.MarkLabel(loop);
            }
          }
          for(int j=0; j<bindings.Length; j++)
          { if(j!=bindings.Length-1) cg.Dup();
            cg.EmitInt(j);
            cg.ILG.Emit(OpCodes.Ldelem_Ref);
            cg.EmitSet(bindings[j]);
          }
          if(useEnd) cg.ILG.MarkLabel(end);
        }
        else if(constValue is object[])
        { object[] values = (object[])constValue;
          for(int j=0; j<bindings.Length; j++)
            cg.EmitSet(bindings[j], values[j] is Node ? (Node)values[j] : new LiteralNode(values[j]));
        }
        else
        { Debug.Assert(bindings.Length==1);
          cg.EmitSet(bindings[0], new LiteralNode(constValue));
        }
      }
      Body.Emit(cg, ref etype);
    }
  }

  public override object Evaluate()
  { if(IsConstant) return Body.Evaluate();

    InterpreterEnvironment ne, old=InterpreterEnvironment.Current;
    try
    { InterpreterEnvironment.Current = ne = new InterpreterEnvironment(old);
      for(int i=0; i<Names.Length; i++)
      { Name[] bindings = Names[i];
        object value = Inits[i].Evaluate();
        if(bindings.Length==1 && !(value is MultipleValues)) ne.Bind(bindings[0].String, value);
        else
        { object[] values = Ops.CheckValues(Ops.ExpectValues(value), bindings.Length);
          for(int j=0; j<bindings.Length; j++) ne.Bind(bindings[j].String, values[j]);
        }
      }
      return Body.Evaluate();
    }
    finally { InterpreterEnvironment.Current=old; }
  }

  public override Type GetNodeType() { return Body.GetNodeType(); }

  public override void MarkTail(bool tail)
  { Tail = tail;
    foreach(Node n in Inits) n.MarkTail(false);
    Body.MarkTail(tail);
  }

  public override void Optimize()
  { bool isconst = Body.IsConstant;
    if(isconst) foreach(Node n in Inits) if(!n.IsConstant) { isconst=false; break; }
    IsConstant = isconst;
  }

  public override void SetFlags()
  { ClearsStack = Body.ClearsStack || HasTryNode(Inits);
    Interrupts  = Body.Interrupts || HasInterrupt(Inits);
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this))
    { foreach(Node n in Inits) n.Walk(w);
      Body.Walk(w);
    }
    w.PostWalk(this);
  }

  public readonly Name[][] Names;
  public readonly Node[] Inits;
  public readonly Node Body;
}
#endregion

#region ValuesNode
public sealed class ValuesNode : Node
{ public ValuesNode(params Node[] values) { Values = values; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(etype==typeof(void)) cg.EmitVoids(Values);
    else
    { cg.EmitObjectArray(Values);
      cg.EmitNew(typeof(MultipleValues), typeof(object[]));
      etype = typeof(MultipleValues);
      TailReturn(cg);
    }
  }

  public override object Evaluate() { return new MultipleValues(MakeObjectArray(Values)); }
  public override Type GetNodeType() { return typeof(MultipleValues); }

  public override void MarkTail(bool tail)
  { Tail = tail;
    foreach(Node n in Values) n.MarkTail(false);
  }

  public override void SetFlags()
  { if(!IsConstant)
    { ClearsStack = HasTryNode(Values);
      Interrupts  = HasInterrupt(Values);
    }
  }

  public override void Optimize() { IsConstant = AreConstant(Values); }

  public override void Walk(IWalker w)
  { if(w.Walk(this)) foreach(Node n in Values) n.Walk(w);
    w.PostWalk(this);
  }

  public readonly Node[] Values;
}
#endregion

#region VariableNode
public sealed class VariableNode : Node
{ public VariableNode(string name) { Name = new Name(name); }
  public VariableNode(Name name) { Name = name; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(Options.Current.Debug && !Name.Type.IsValueType) // FIXME: add use-of-unassigned-variable check for value types
    { cg.EmitGet(Name);
      cg.Dup();
      cg.EmitString(Name.String);
      cg.EmitCall(typeof(Ops), "CheckVariable");
      etype = Name.Type;
    }
    else if(etype!=typeof(void))
    { cg.EmitGet(Name);
      EmitTryConvert(cg, Name.Type, ref etype);
    }
    if(Tail) Debug.Assert(!etype.IsValueType);
    TailReturn(cg);
  }

  public override object Evaluate()
  { object value;
    if(Name.Depth==Name.Global) value = TopLevel.Current.Get(Name.String);
    else
    { InterpreterEnvironment cur = InterpreterEnvironment.Current;
      value = cur==null ? TopLevel.Current.Get(Name.String) : cur.Get(Name.String);
    }
    Ops.CheckVariable(value, Name.String);
    return value;
  }

  public override Type GetNodeType() { return Name.Type; }

  public Name Name;
}
#endregion

#region VectorNode
public sealed class VectorNode : Node
{ public VectorNode(Node[] items) { Items = items; }

  public override void Emit(CodeGenerator cg, ref Type etype)
  { if(etype==typeof(void))
    { cg.MarkPosition(this);
      cg.EmitVoids(Items);
    }
    else
    { if(IsConstant) cg.EmitConstantObject(Evaluate());
      else
      { cg.MarkPosition(this);
        cg.EmitObjectArray(Items);
      }
      etype = typeof(object[]);
    }
    TailReturn(cg);
  }

  public override object Evaluate() { return MakeObjectArray(Items); }
  public override Type GetNodeType() { return typeof(object[]); }

  public override void MarkTail(bool tail)
  { Tail = tail;
    foreach(Node node in Items) node.MarkTail(false);
  }

  public override void Optimize()
  { bool isconst = true;
    foreach(Node node in Items) if(!node.IsConstant) { isconst=false; break; }
    IsConstant = isconst;
  }

  public override void SetFlags()
  { ClearsStack = HasTryNode(Items);
    Interrupts  = HasInterrupt(Items);
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this)) foreach(Node n in Items) n.Walk(w);
    w.PostWalk(this);
  }

  public readonly Node[] Items;
}
#endregion

#region WrapperNode
public class WrapperNode : Node
{ public WrapperNode() { }
  public WrapperNode(Node node) { Node = node; }

  public override void Emit(CodeGenerator cg, ref Type etype) { Node.Emit(cg, ref etype); }
  public override object Evaluate() { return Node.Evaluate(); }
  public override Type GetNodeType() { return Node.GetNodeType(); }
  public override void MarkTail(bool tail) { Node.MarkTail(tail); }
  public override void Optimize() { IsConstant = Node.IsConstant; }

  public override void SetFlags()
  { ClearsStack = Node.ClearsStack;
    Interrupts  = Node.Interrupts;
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this)) Node.Walk(w);
    w.PostWalk(this);
  }

  public Node Node;
}
#endregion

} // namespace Scripting
