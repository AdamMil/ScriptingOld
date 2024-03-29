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
using Scripting.Backend;

namespace Scripting
{

public abstract class Snippet
{ public object Run() { return Run(null); }
  protected abstract object Run(LocalEnvironment ENV);
}

public delegate object SnippetDelegate(LocalEnvironment env);

public sealed class DynamicSnippet : Snippet
{ public DynamicSnippet(DynamicMethod dm, Binding[] bindings, object[] constants)
  { Proc = (SnippetDelegate)dm.CreateDelegate(typeof(SnippetDelegate), this);
    Bindings  = bindings;
    Constants = constants;
  }

  protected override object Run(LocalEnvironment env) { return Proc(env); }

  public readonly SnippetDelegate Proc;
  readonly object[] Bindings;
  readonly object[] Constants;
}

public static class SnippetMaker
{ public static void DumpAssembly()
  { Assembly.Save();
    string bn = "snippets"+index.Next;
    Assembly = new AssemblyGenerator(bn, bn+".dll");
  }

  public static Snippet Generate(LambdaNode body) { return Assembly.GenerateSnippet(body); }
  public static Snippet Generate(LambdaNode body, string typeName)
  { return Assembly.GenerateSnippet(body, typeName);
  }

  public static DynamicSnippet GenerateDynamic(LambdaNode body) { return Assembly.GenerateDynamicSnippet(body); }

  public static AssemblyGenerator Assembly = new AssemblyGenerator("snippets", "snippets.dll", true);

  static Index index = new Index();
}

} // namespace Scripting
