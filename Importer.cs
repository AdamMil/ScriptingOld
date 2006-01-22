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
using System.IO;
using System.Reflection;

namespace Scripting
{

public sealed class Importer
{ Importer() { }

  public static void Import(TopLevel top, IDictionary dict, TopLevel env,
                            string[] names, string[] asNames, string myName)
  { if(names==null)
      foreach(DictionaryEntry de in dict) top.Globals.Bind((string)de.Key, de.Value, env);
    else
      for(int i=0; i<names.Length; i++)
      { object obj = dict[names[i]];
        if(obj==null && !dict.Contains(names[i]))
          throw new ArgumentException(myName+" does not contain a member called '"+names[i]+"'");
        top.Globals.Bind(asNames[i], obj, env);
      }
  }

  public static MemberContainer Load(string name) { return Load(name, true, false); }
  public static MemberContainer Load(string name, bool throwOnError) { return Load(name, throwOnError, false); }
  public static MemberContainer Load(string name, bool throwOnError, bool returnTop)
  { MemberContainer module, top;
    bool returnNow = false;

    lock(LoadedModules) module = (MemberContainer)LoadedModules[name];
    if(module!=null) return module;

    // FIXME: optimize this so loading a dotted name from a file doesn't compile the file multiple times
    string[] bits = name.Split('.');
    top = LoadFromPath(bits[0]);
    if(top==null) top = LoadBuiltin(bits[0]);
    if(top==null)
    { top = LoadFromDotNet(bits, returnTop);
      if(top!=null) returnNow = true;
    }

    if(top!=null) lock(LoadedModules) LoadedModules[bits[0]] = top;
    if(returnNow) return top;

    module = top;
    for(int i=1; i<bits.Length && module!=null; i++)
    { object obj;
      if(!module.GetProperty(module, bits[i], out obj)) goto error;
      module = obj as MemberContainer;
    }
    if(returnTop) module = top;
    if(module!=null || !throwOnError) return module;

    error: throw new TypeLoadException("Unable to load module: "+name);
  }

  public static MemberContainer Load(Type type)
  { MemberContainer module = (MemberContainer)builtinTypes[type];
    if(module==null) builtinTypes[type] = module = ModuleGenerator.Generate(type);
    return module;
  }

  public static Hashtable LoadedModules = new Hashtable();

  static MemberContainer LoadBuiltin(string name)
  { string ns = Options.Current.Language.BuiltinsNamespace;
    if(ns==null) return null;

    if(builtinNames==null)
    { builtinNames = new SortedList();
      foreach(Type type in Options.Current.Language.GetType().Assembly.GetTypes())
        if(type.IsPublic && type.Namespace==ns)
        { object[] attrs = type.GetCustomAttributes(typeof(ScriptNameAttribute), false);
          if(attrs.Length!=0) builtinNames[((ScriptNameAttribute)attrs[0]).Name] = type;
        }
    }

    { Type type = (Type)builtinNames[name];
      if(type==null) type = Interop.GetType(ns+"."+name);
      return type==null ? null : Load(type);
    }
  }

  static MemberContainer LoadFromDotNet(string[] bits, bool returnTop)
  { ReflectedNamespace rns = ReflectedNamespace.FromName(bits, returnTop);
    return rns.GetMemberNames().Count==0 ? null : rns;
  }

  static MemberContainer LoadFromPath(string name) { return null; } // TODO: implement this

  static readonly Hashtable builtinTypes=new Hashtable();
  static SortedList builtinNames;
}

} // namespace Scripting