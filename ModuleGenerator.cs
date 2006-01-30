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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-13037, USA.
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
  { ReflectedType.FromType(type, false).Export(TopLevel, true);
  }
}

public static class ModuleGenerator
{ /*
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
    else
    { Language lang = ((ScriptCodeAttribute)attrs[0]).Language;
      foreach(ScriptCodeAttribute sc in attrs)
        if(sc.Language!=lang) throw new NotSupportedException("Cannot mix languages in a single module.");
    }

    Array.Sort(attrs, CodeAttrComparer.Instance);

    // TODO: provide some versioning support (eg, add the assembly's full name to the filename somehow)
    string dllName = "type-"+type.FullName.Replace('+', '%')+".dll", filename;
    #if !DEBUG
    filename = Path.Combine(Scripting.SystemDllCache, dllName);
    MemberContainer mc = LoadFromCache(filename);
    if(mc!=null) return mc;
    #endif
    filename = Scripting.UserDllCache;
    if(!Directory.Exists(filename)) try { Directory.CreateDirectory(filename); } catch { }
    filename = Path.Combine(filename, dllName);
    #if !DEBUG
    mc = LoadFromCache(filename);
    if(mc!=null) return mc;
    #endif

    AssemblyGenerator ag = new AssemblyGenerator(type.FullName, filename);
    TopLevel oldTL = TopLevel.Current;
    try
    { TopLevel.Current = new TopLevel();
      Options.Save();

      Options.Current.Language = ((ScriptCodeAttribute)attrs[0]).Language;
      bool addBuiltins = Options.Current.Language.ShouldAddBuiltins(type);

      Options.Current.Debug = Options.Current.DebugModules;
      Options.Current.Optimize = OptimizeType.Speed;
      Options.Current.IsPreCompilation = false;

      if(addBuiltins) Options.Current.Language.Builtins.Export(TopLevel.Current); // TODO: affected by above FIXME
      ReflectedType.FromType(type, false).Export(TopLevel.Current);

      TypeGenerator tg = ag.DefineType("ScriptModule", typeof(BuiltinModule));
      CodeGenerator cg;

      cg = tg.GetInitializer();
      Slot topSlot = tg.DefineStaticField(FieldAttributes.Private, "topLevel", typeof(TopLevel));
      Slot oldInitTop = cg.AllocLocalTemp(typeof(TopLevel));

      cg.EmitFieldGet(typeof(TopLevel), "Current");
      oldInitTop.EmitSet(cg);
      cg.ILG.BeginExceptionBlock();
      cg.EmitNew(typeof(TopLevel));
      topSlot.EmitSet(cg);
      if(addBuiltins)
      { cg.EmitLanguage(Options.Current.Language);
        cg.EmitPropGet(typeof(Language), "Builtins");
        topSlot.EmitGet(cg);
        cg.EmitCall(typeof(MemberContainer), "Export", typeof(TopLevel));
      }
      topSlot.EmitGet(cg);
      cg.EmitFieldSet(typeof(TopLevel), "Current");

      cg = tg.DefineStaticMethod(MethodAttributes.Private, "Run", typeof(void), typeof(LocalEnvironment));
      MethodBuilder mb = (MethodBuilder)cg.MethodBase;
      mb.DefineParameter(1, ParameterAttributes.In, "ENV");

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
      
      cg.EmitReturn();
      cg.Finish();

      MethodInfo run = (MethodInfo)cg.MethodBase;

      #region Constructor
      cg = tg.DefineConstructor(Type.EmptyTypes);
      cg.EmitThis();
      topSlot.EmitGet(cg);
      cg.EmitTypeOf(type);
      cg.EmitCall(typeof(BuiltinModule).GetConstructor(new Type[] { typeof(TopLevel), typeof(Type) }));

      Slot oldConsTop = cg.AllocLocalTemp(typeof(TopLevel));
      cg.EmitFieldGet(typeof(TopLevel), "Current");
      oldConsTop.EmitSet(cg);
      cg.ILG.BeginExceptionBlock();
      topSlot.EmitGet(cg);
      cg.EmitFieldSet(typeof(TopLevel), "Current");
      cg.EmitNull();
      cg.EmitCall(run);
      cg.ILG.BeginFinallyBlock();
      oldConsTop.EmitGet(cg);
      cg.FreeLocalTemp(oldConsTop);
      cg.EmitFieldSet(typeof(TopLevel), "Current");
      cg.ILG.EndExceptionBlock();

      cg.EmitReturn();
      cg.Finish();
      #endregion

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
    filename = Path.GetFullPath(filename).Trim();

    DateTime modTime = File.GetLastWriteTimeUtc(filename);
    MemberContainer mc = LoadFromCache(filename+".dll", modTime);
    if(mc!=null) return mc;

    string dllPath = Scripting.UserDllCache;
    try
    { if(!Directory.Exists(dllPath)) Directory.CreateDirectory(dllPath);
      // SHA1 has a slim chance of a collision, and i don't like it, but it's much lower than the chance of the path
      // exceeding MAX_PATH if i do some kind of direct translation of the source filename
      // TODO: perhaps we should cache more than the file name. ie, maybe we should also cache the content
      // of the script file... but that slows things down.
      byte[] hash = new System.Security.Cryptography.SHA1Managed()
                          .ComputeHash(System.Text.Encoding.UTF8.GetBytes(filename));
      dllPath = Path.Combine(dllPath, Path.GetFileName(filename)+"-"+Ops.ToHex(hash)+".dll");

      mc = LoadFromCache(dllPath, modTime);
      if(mc!=null) return mc;
    }
    catch { }

    Language oldLang = Options.Current.Language;
    try
    { Options.Current.Language = Scripting.LoadLanguage(Path.GetExtension(filename));
      Type type = Generate("ScriptModule", Path.GetFileNameWithoutExtension(dllPath), dllPath,
                           AST.CreateCompiled(Options.Current.Language.ParseFile(filename)), PEFileKinds.Dll, modTime);
      return (MemberContainer)type.GetConstructor(Type.EmptyTypes).Invoke(null);
    }
    finally { Options.Current.Language = oldLang; }
  }
  #endregion

  #region Generate .DLL from LambdaNode
  public static Type Generate(string typeName, string filename, LambdaNode body, PEFileKinds fileKind)
  { return Generate(typeName, typeName, filename, body, fileKind, null);
  }
  public static Type Generate(string typeName, string filename, LambdaNode body, PEFileKinds fileKind,
                              DateTime? fileTime)
  { return Generate(typeName, typeName, filename, body, fileKind, fileTime);
  }
  public static Type Generate(string typeName, string moduleName, string filename, LambdaNode body, 
                              PEFileKinds fileKind)
  { return Generate(typeName, moduleName, filename, body, fileKind, null);
  }
  public static Type Generate(string typeName, string moduleName, string filename, LambdaNode body,
                              PEFileKinds fileKind, DateTime? fileTime)
  { TopLevel oldTop = TopLevel.Current;
    try
    { TopLevel.Current = new TopLevel();
      MemberContainer builtins = Options.Current.Language.Builtins;
      if(builtins!=null) builtins.Export(TopLevel.Current);

      AssemblyGenerator ag = new AssemblyGenerator(moduleName, filename);
      TypeGenerator tg = ag.DefineType(TypeAttributes.Public|TypeAttributes.Sealed, typeName, typeof(CodeModule));
      Slot topSlot = tg.DefineStaticField(FieldAttributes.Private, "topLevel", typeof(TopLevel));

      if(fileTime.HasValue)
        tg.TypeBuilder.DefineField("fileTime", typeof(long), FieldAttributes.Literal|FieldAttributes.Public|FieldAttributes.Static)
          .SetConstant(fileTime.Value.ToFileTimeUtc());

      #region Initializer (start)
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
        cg.EmitCall(typeof(MemberContainer), "Export", typeof(TopLevel));
      }
      #endregion

      #region Run
      cg = tg.DefineStaticMethod(MethodAttributes.Private, "Run", typeof(object), typeof(LocalEnvironment));
      MethodBuilder mb = (MethodBuilder)cg.MethodBase;
      mb.DefineParameter(1, ParameterAttributes.In, "ENV");
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
      { ConstructorInfo cons = (ConstructorInfo)cg.MethodBase;
        cg = tg.DefineStaticMethod("Main", typeof(void));
        cg.EmitNew(cons);
        cg.ILG.Emit(OpCodes.Pop);
        cg.EmitReturn();
        cg.Finish();
        main = (MethodInfo)cg.MethodBase;
      }
      #endregion

      #region Initializer (continued)
      cg = tg.GetInitializer();
      cg.ILG.BeginFinallyBlock();
      oldTopSlot.EmitGet(cg);
      cg.FreeLocalTemp(oldTopSlot);
      cg.EmitFieldSet(typeof(TopLevel), "Current");
      cg.ILG.EndExceptionBlock();
      #endregion

      Type ret = tg.FinishType();

      if(fileKind!=PEFileKinds.Dll)
      { ag.Assembly.SetEntryPoint(main, fileKind);
        if(ag.IsDebug) ag.Module.SetUserEntryPoint(run);
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

  #region TestLoader
  sealed class TestLoader : MarshalByRefObject
  { public bool TryLoad(string dllPath, DateTime? fileTime)
    { try
      { Assembly ass = Assembly.LoadFrom(dllPath);
        Type type = ass.GetType("ScriptModule");
        if(type!=null && type.IsSubclassOf(typeof(MemberContainer)))
        { if(fileTime.HasValue)
          { FieldInfo fi = type.GetField("fileTime", BindingFlags.Static|BindingFlags.Public);
            if(fi==null || fi.FieldType!=typeof(long) || (long)fi.GetValue(null)!=fileTime.Value.ToFileTimeUtc())
              return false;
          }
          return type.GetConstructor(Type.EmptyTypes)!=null;
        }
      }
      catch { }
      return false;
    }
  }
  #endregion

  static MemberContainer LoadFromCache(string dllPath) { return LoadFromCache(dllPath, null); }
  static MemberContainer LoadFromCache(string dllPath, DateTime? fileTime)
  { if(File.Exists(dllPath))
    { AppDomain domain = AppDomain.CreateDomain("Scripting Loader Domain");
      try
      { if(((TestLoader)domain.CreateInstanceFromAndUnwrap(Assembly.GetExecutingAssembly().Location,
                                                           typeof(TestLoader).FullName))
             .TryLoad(dllPath, fileTime))
          return Assembly.LoadFrom(dllPath).GetType("ScriptModule").GetConstructor(Type.EmptyTypes)
                         .Invoke(null) as MemberContainer;
      }
      #if DEBUG
      catch(Exception e) { Console.WriteLine("LOAD: "+e.ToString()); }
      #else
      catch { }
      #endif
      finally { AppDomain.Unload(domain); }
    }
    return null;
  }

  static Index index = new Index();
}

} // namespace Scripting.Backend