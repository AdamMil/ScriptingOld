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
using System.IO;
using System.Reflection;
using System.Reflection.Emit;

namespace Scripting.Backend
{

public abstract class BuiltinModule : CodeModule
{ public BuiltinModule(TopLevel top, Type type) : base(type.FullName, top)
  { ReflectedType.FromType(type, false).Import(TopLevel, true);
  }
}

public sealed class ModuleGenerator
{ ModuleGenerator() { }

  /*
  #region Generate from a ModuleNode
  public static Module Generate(ModuleNode mod)
  { TypeGenerator tg = SnippetMaker.Assembly.DefineType("module"+index.Next+"$"+mod.Name, typeof(Module));

    CodeGenerator cg = tg.DefineStaticMethod(MethodAttributes.Private, "Run", typeof(object),
                                             new Type[] { typeof(LocalEnvironment) });
    cg.SetupNamespace(mod.MaxNames);
    mod.Body.Emit(cg);
    cg.Finish();

    MethodBase run = cg.MethodBase;
    cg = tg.DefineConstructor(Type.EmptyTypes);
    cg.EmitThis();
    cg.EmitString(mod.Name);
    cg.EmitCall(typeof(Module).GetConstructor(new Type[] { typeof(string) }));

    cg.EmitThis();
    EmitExports(cg, mod.Exports);
    cg.EmitFieldSet(typeof(Module), "Exports");

    Slot old = cg.AllocLocalTemp(typeof(TopLevel));
    cg.EmitFieldGet(typeof(TopLevel), "Current");
    old.EmitSet(cg);
    cg.ILG.BeginExceptionBlock();
    cg.EmitPropGet(typeof(Builtins), "Instance");
    cg.EmitThis();
    cg.EmitFieldGet(typeof(Module), "TopLevel");
    cg.Dup();
    cg.EmitFieldSet(typeof(TopLevel), "Current");
    cg.EmitCall(typeof(Module), "ImportAll");
    cg.EmitNull();
    cg.EmitCall((MethodInfo)run);
    cg.ILG.Emit(OpCodes.Pop);
    cg.ILG.BeginFinallyBlock();
    old.EmitGet(cg);
    cg.EmitFieldSet(typeof(TopLevel), "Current");
    cg.ILG.EndExceptionBlock();
    cg.EmitReturn();
    cg.Finish();

    return (Module)tg.FinishType().GetConstructor(Type.EmptyTypes).Invoke(null);
  }
  #endregion
  */

  #region Generate from builtin type
  public static MemberContainer Generate(Type type)
  { object[] attrs = type.GetCustomAttributes(typeof(ScriptCodeAttribute), false);
    if(attrs.Length==0) return ReflectedType.FromType(type, false);
    Array.Sort(attrs, CodeAttrComparer.Instance);

    // TODO: come up with a better naming scheme (replacing '+' with '.' can create collisions)
    string filename = Path.Combine(Scripting.DllCache, type.FullName.Replace('+', '.')+".dll");
    MemberContainer mc = LoadFromCache(Scripting.DllCache);
    if(mc!=null) return mc;

    AssemblyGenerator ag = new AssemblyGenerator(type.FullName, filename);
    TopLevel oldTL = TopLevel.Current;
    try
    { TopLevel.Current = new TopLevel();
      Options.Save();
      bool addBuiltins = Options.Current.Language.ShouldAddBuiltins(type); // FIXME: figure out how to handle Language changing at runtime

      Options.Current.Debug = Options.Current.DebugModules;
      Options.Current.Optimize = OptimizeType.Speed;
      Options.Current.IsPreCompilation = false;

      if(addBuiltins) Options.Current.Language.Builtins.Import(TopLevel.Current); // TODO: affected by above FIXME
      ReflectedType.FromType(type, false).Import(TopLevel.Current);

      TypeGenerator tg = ag.DefineType(CacheTypeName(), typeof(BuiltinModule));
      CodeGenerator cg;

      cg = tg.GetInitializer();
      Slot staticTop = tg.DefineStaticField(FieldAttributes.Private, "topLevel", typeof(TopLevel));
      Slot oldInitTop = cg.AllocLocalTemp(typeof(TopLevel));

      cg.EmitFieldGet(typeof(TopLevel), "Current");
      oldInitTop.EmitSet(cg);
      cg.ILG.BeginExceptionBlock();
      cg.EmitNew(typeof(TopLevel));
      staticTop.EmitSet(cg);
      if(addBuiltins)
      { cg.EmitLanguage(Options.Current.Language);
        cg.EmitPropGet(typeof(Language), "Builtins");
        staticTop.EmitGet(cg);
        cg.EmitCall(typeof(MemberContainer), "Import", typeof(TopLevel));
      }
      staticTop.EmitGet(cg);
      cg.EmitFieldSet(typeof(TopLevel), "Current");

      cg = tg.DefineStaticMethod(MethodAttributes.Private, "Run", typeof(void),
                                 new Type[] { typeof(LocalEnvironment), typeof(TopLevel) });
      Slot oldRunTop = cg.AllocLocalTemp(typeof(TopLevel));
      Slot topSlot = new ArgSlot((MethodBuilder)cg.MethodBase, 1, "topLevel", typeof(TopLevel));
      cg.EmitFieldGet(typeof(TopLevel), "Current");
      oldRunTop.EmitSet(cg);
      cg.ILG.BeginExceptionBlock();
      topSlot.EmitGet(cg);
      cg.EmitFieldSet(typeof(TopLevel), "Current");

      int closedVars = 0;
      LambdaNode[] nodes = new LambdaNode[attrs.Length];
      string sourceName = "<"+type.Name+" module>";
      for(int i=0; i<attrs.Length; i++)
      { ScriptCodeAttribute attr = (ScriptCodeAttribute)attrs[i];
        nodes[i] = AST.CreateCompiled(attr.Language.Parse(sourceName, attr.Code));
        if((attr.RunAt&RunAt.CompileTime)!=0)
        { Options.Current.IsPreCompilation = true;
          SnippetMaker.Generate(nodes[i]).Run(null);
          Options.Current.IsPreCompilation = false;
        }
        if((attr.RunAt&RunAt.Runtime)!=0) closedVars = Math.Max(closedVars, nodes[i].ClosedVars);
      }

      cg.SetupNamespace(closedVars, topSlot);

      for(int i=0; i<attrs.Length; i++)
      { ScriptCodeAttribute attr = (ScriptCodeAttribute)attrs[i];
        if((attr.RunAt&RunAt.Runtime)!=0)
        { nodes[i].Body.MarkTail(false);
          nodes[i].Body.EmitVoid(cg);
        }
      }
      
      cg.ILG.BeginFinallyBlock();
      oldRunTop.EmitGet(cg);
      cg.EmitFieldSet(typeof(TopLevel), "Current");
      cg.ILG.EndExceptionBlock();
      cg.EmitReturn();
      cg.FreeLocalTemp(oldRunTop);
      cg.Finish();

      MethodInfo run = (MethodInfo)cg.MethodBase;

      cg = tg.DefineConstructor(Type.EmptyTypes);
      cg.EmitThis();
      staticTop.EmitGet(cg);
      cg.EmitTypeOf(type);
      cg.EmitCall(typeof(BuiltinModule).GetConstructor(new Type[] { typeof(TopLevel), typeof(Type) }));
      cg.EmitNull();
      cg.EmitThis();
      cg.EmitFieldGet(typeof(CodeModule), "TopLevel");
      cg.EmitCall(run);
      cg.EmitReturn();
      cg.Finish();

      cg = tg.GetInitializer();
      cg.ILG.BeginFinallyBlock();
      oldInitTop.EmitGet(cg);
      cg.EmitFieldSet(typeof(TopLevel), "Current");
      cg.ILG.EndExceptionBlock();
      cg.FreeLocalTemp(oldInitTop);

      type = tg.FinishType();
      try { ag.Save(); } catch { }

      return (MemberContainer)type.GetConstructor(Type.EmptyTypes).Invoke(null);
    }
    finally { TopLevel.Current=oldTL; Options.Restore(); }
  }
  #endregion

  #region Generate from file
  public static MemberContainer Generate(string filename)
  { if(!File.Exists(filename)) return null;
    filename = Path.GetFullPath(filename);

    string dllPath = Path.Combine(Path.GetDirectoryName(filename), Path.GetFileNameWithoutExtension(filename)+".dll");
    DateTime modTime = File.GetLastWriteTimeUtc(filename);
    MemberContainer mc = LoadFromCache(dllPath, modTime);
    if(mc!=null) return mc;

    Language oldLang = Options.Current.Language;
    try
    { Options.Current.Language = Scripting.LoadLanguage(Path.GetExtension(filename));
      Type type = Generate(CacheTypeName(modTime), Path.GetFileNameWithoutExtension(filename), dllPath,
                           AST.CreateCompiled(Options.Current.Language.ParseFile(filename)), PEFileKinds.Dll);
      return (MemberContainer)type.GetConstructor(Type.EmptyTypes).Invoke(null);
    }
    finally { Options.Current.Language = oldLang; }
  }
  #endregion

  #region Generate .DLL from node
  public static Type Generate(string typeName, string moduleName, string filename, LambdaNode body, PEFileKinds fileKind)
  { TopLevel oldTop = TopLevel.Current;
    try
    { TopLevel.Current = new TopLevel();
      MemberContainer builtins = Options.Current.Language.Builtins;
      if(builtins!=null) builtins.Import(TopLevel.Current);

      AssemblyGenerator ag = new AssemblyGenerator(moduleName, filename);
      TypeGenerator tg = ag.DefineType(TypeAttributes.Public|TypeAttributes.Sealed, typeName, typeof(CodeModule));
      Slot topSlot = tg.DefineStaticField(FieldAttributes.Private, "topLevel", typeof(TopLevel));

      CodeGenerator cg = tg.GetInitializer();
      cg.EmitFieldGet(typeof(TopLevel), "Current");
      Slot oldTopSlot = cg.AllocLocalTemp(typeof(TopLevel));
      oldTopSlot.EmitSet(cg);

      cg.ILG.BeginExceptionBlock();
      cg.EmitNew(typeof(TopLevel));
      cg.Dup();
      topSlot.EmitSet(cg);
      cg.EmitFieldSet(typeof(TopLevel), "Current");

      if(builtins!=null)
      { cg.EmitLanguage(Options.Current.Language);
        cg.EmitPropGet(typeof(Language), "Builtins");
        topSlot.EmitGet(cg);
        cg.EmitCall(typeof(MemberContainer), "Import", typeof(TopLevel));
      }

      #region Run
      cg = tg.DefineStaticMethod(MethodAttributes.Private, "Run", typeof(object), typeof(LocalEnvironment));
      cg.SetupNamespace(body.ClosedVars);
      body.Body.Emit(cg);
      cg.Finish();
      #endregion
      MethodInfo run = (MethodInfo)cg.MethodBase;

      #region Constructor
      cg = tg.DefineConstructor();
      cg.EmitThis();
      cg.EmitString(moduleName);
      topSlot.EmitGet(cg);
      cg.EmitCall(typeof(CodeModule).GetConstructor(new Type[] { typeof(string), typeof(TopLevel) }));

      cg.EmitFieldGet(typeof(TopLevel), "Current");
      Slot oldTop2 = cg.AllocLocalTemp(typeof(TopLevel));
      oldTop2.EmitSet(cg);
      cg.ILG.BeginExceptionBlock();
      topSlot.EmitGet(cg);
      cg.EmitFieldSet(typeof(TopLevel), "Current");
      cg.EmitNull();
      cg.EmitCall(run);
      cg.ILG.Emit(OpCodes.Pop);
      cg.ILG.BeginFinallyBlock();
      oldTop2.EmitGet(cg);
      cg.FreeLocalTemp(oldTop2);
      cg.EmitFieldSet(typeof(TopLevel), "Current");
      cg.ILG.EndExceptionBlock();
      cg.EmitReturn();
      cg.Finish();
      #endregion

      MethodInfo main = null;
      #region Main
      if(fileKind!=PEFileKinds.Dll)
      { cg = tg.DefineStaticMethod("Main", typeof(void));
        cg.EmitNew((ConstructorInfo)cg.MethodBase);
        cg.ILG.Emit(OpCodes.Pop);
        cg.EmitReturn();
        cg.Finish();
        main = (MethodInfo)cg.MethodBase;
      }
      #endregion

      cg = tg.GetInitializer();
      cg.ILG.BeginFinallyBlock();
      oldTopSlot.EmitGet(cg);
      cg.FreeLocalTemp(oldTopSlot);
      cg.EmitFieldSet(typeof(TopLevel), "Current");
      cg.ILG.EndExceptionBlock();

      Type ret = tg.FinishType();

      if(fileKind!=PEFileKinds.Dll)
      { ag.Assembly.SetEntryPoint(main, fileKind);
        if(Options.Current.Debug) ag.Module.SetUserEntryPoint(run);
      }

      ag.Save();
      return ret;
    }
    finally { TopLevel.Current = oldTop; }
  }
  #endregion

  #region CodeAttrComparer
  sealed class CodeAttrComparer : System.Collections.IComparer
  { CodeAttrComparer() { }

    public int Compare(object a, object b)
    { ScriptCodeAttribute ca=(ScriptCodeAttribute)a, cb=(ScriptCodeAttribute)b;

      // CompileTime < CompileTime|Runtime < Runtime
      if(ca.RunAt==RunAt.CompileTime)
      { if(cb.RunAt!=RunAt.CompileTime) return -1;
      }
      else if(ca.RunAt==RunAt.Both)
      { if(cb.RunAt==RunAt.CompileTime) return 1;
        if(cb.RunAt==RunAt.Runtime) return -1;
      }
      else if(cb.RunAt!=RunAt.Runtime) return 1;

      return ca.Order-cb.Order;
    }
    
    public static readonly CodeAttrComparer Instance = new CodeAttrComparer();
  }
  #endregion

  static string CacheTypeName() { return "ScriptModule"; }
  static string CacheTypeName(DateTime time) { return "ScriptModule_"+time.ToFileTimeUtc(); }

  static MemberContainer LoadFromCache(string dllPath) { return LoadFromCache(dllPath, CacheTypeName()); }
  static MemberContainer LoadFromCache(string dllPath, DateTime date)
  { return LoadFromCache(dllPath, CacheTypeName(date));
  }
  static MemberContainer LoadFromCache(string dllPath, string typeName)
  { 
    #if !DEBUG
    if(File.Exists(dllPath))
      try
      { Assembly ass = Assembly.LoadFrom(dllPath);
        Type mtype = ass.GetType(typeName);
        if(mtype!=null && mtype.IsSubclassOf(typeof(MemberContainer)))
          return (MemberContainer)mtype.GetConstructor(Type.EmptyTypes).Invoke(null);
      }
      catch { }
    #endif
    return null;
  }

  static Index index = new Index();
}

} // namespace Scripting.Backend