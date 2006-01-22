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
using System.Reflection;
using System.Reflection.Emit;
using System.Diagnostics;
using System.Diagnostics.SymbolStore;

namespace Scripting
{

public sealed class AssemblyGenerator
{ public AssemblyGenerator(string moduleName) : this(moduleName, Options.Current.Debug) { }
  public AssemblyGenerator(AppDomain domain, string moduleName) : this(domain, moduleName, Options.Current.Debug) { }
  public AssemblyGenerator(string moduleName, bool debug) : this(AppDomain.CurrentDomain, moduleName, debug) { }
  public AssemblyGenerator(AppDomain domain, string moduleName, bool debug)
    : this(domain, moduleName, "_assembly"+index.Next.ToString()+".dll", debug) { }
  public AssemblyGenerator(string moduleName, string outFileName)
    : this(moduleName, outFileName, Options.Current.Debug) { }
  public AssemblyGenerator(string moduleName, string outFileName, bool debug)
    : this(AppDomain.CurrentDomain, moduleName, outFileName, debug) { }
  public AssemblyGenerator(AppDomain domain, string moduleName, string outFileName, bool debug)
  { string dir = System.IO.Path.GetDirectoryName(outFileName);
    if(dir=="") dir=null;
    outFileName = System.IO.Path.GetFileName(outFileName);

    AssemblyName an = new AssemblyName();
    an.Name  = moduleName;
    IsDebug  = debug;
    Assembly = domain.DefineDynamicAssembly(an, AssemblyBuilderAccess.RunAndSave, dir, null, null, null, null, true);

    if(debug)
    { /* .NET 2.0 ConstructorInfo ci =
        typeof(DebuggableAttribute).GetConstructor(new Type[] { typeof(DebuggableAttribute.DebuggingModes) });
      CustomAttributeBuilder ab = new CustomAttributeBuilder(ci,  
        new object[] { DebuggableAttribute.DebuggingModes.DisableOptimizations |
                       DebuggableAttribute.DebuggingModes.Default });*/
      ConstructorInfo ci = typeof(DebuggableAttribute).GetConstructor(new Type[] { typeof(bool), typeof(bool) });
      Assembly.SetCustomAttribute(new CustomAttributeBuilder(ci, new object[] { true, true }));
    }

    Module = Assembly.DefineDynamicModule(outFileName, outFileName, debug);
    OutFileName = outFileName;
  }

  public TypeGenerator DefineType(string name) { return DefineType(TypeAttributes.Public, name, null); }
  public TypeGenerator DefineType(string name, Type parent)
  { return DefineType(TypeAttributes.Public, name, parent);
  }
  public TypeGenerator DefineType(TypeAttributes attrs, string name) { return DefineType(attrs, name, null); }
  public TypeGenerator DefineType(TypeAttributes attrs, string name, Type parent)
  { return new TypeGenerator(this, Module.DefineType(name, attrs, parent));
  }

  public Snippet GenerateSnippet(LambdaNode body) { return GenerateSnippet(body, "code_"+index.Next); }
  public Snippet GenerateSnippet(LambdaNode body, string typeName)
  { TypeGenerator tg = DefineType(TypeAttributes.Public|TypeAttributes.Sealed, typeName, typeof(Snippet));
    CodeGenerator cg = tg.DefineMethodOverride("Run", true);
    cg.SetupNamespace(body.ClosedVars);
    body.Body.Emit(cg);
    cg.Finish();
    return (Snippet)tg.FinishType().GetConstructor(Type.EmptyTypes).Invoke(null);
  }

  public void Save() { Assembly.Save(OutFileName); }

  public readonly AssemblyBuilder Assembly;
  public readonly ModuleBuilder   Module;
  public ISymbolDocumentWriter Symbols;
  public readonly string OutFileName;
  public readonly bool IsDebug;

  static Index index = new Index();
}

} // namespace Scripting
