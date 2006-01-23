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
using System.Collections.Specialized;
using System.IO;
using System.Reflection;
using Scripting.Backend;

namespace Scripting
{

public sealed class Importer
{ Importer() { }
  static Importer() { SearchPaths.Add("."); }

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

    string[] bits = name.Split('.');

    lock(LoadedModules)
    { top = (MemberContainer)LoadedModules[bits[0]];
      if(top==null)
      { top = LoadFromPath(bits[0]);
        if(top==null) top = LoadBuiltin(bits[0]);
        if(top==null)
        { top = LoadFromDotNet(bits);
          if(top!=null && returnTop) returnNow = true;
        }

        if(top!=null) lock(LoadedModules) LoadedModules[bits[0]] = top;
      }
    }

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

  public static readonly ArrayList SearchPaths = new ArrayList();
  public static readonly Hashtable LoadedModules = new Hashtable();

  static MemberContainer LoadBuiltin(string name)
  { string ns = Ops.GetCurrentLanguage().BuiltinsNamespace;
    if(ns==null) return null;

    IDictionary dict;
    lock(builtinNamespaces)
    { dict = (IDictionary)builtinNamespaces[ns];
      if(dict==null)
      { builtinNamespaces[ns] = dict = new SortedList(4);
        foreach(Type type in Ops.GetCurrentLanguage().GetType().Assembly.GetTypes())
          if(type.IsPublic && type.Namespace==ns)
          { object[] attrs = type.GetCustomAttributes(typeof(ScriptNameAttribute), false);
            if(attrs.Length!=0) dict[((ScriptNameAttribute)attrs[0]).Name] = type;
          }
      }
    }

    { Type type = (Type)dict[name];
      if(type==null)
      { name = ns+"."+name;
        type = Ops.GetCurrentLanguage().GetType().Assembly.GetType(name);
        if(type==null) type = Type.GetType(name);
      }
      return type==null ? null : Load(type);
    }
  }

  static MemberContainer LoadFromDotNet(string[] bits)
  { ReflectedNamespace rns = ReflectedNamespace.FromName(bits, true);
    return rns.GetMemberNames().Count==0 ? null : rns;
  }

  static MemberContainer LoadFromPath(string bit)
  { foreach(string search in SearchPaths)
    { string dir=search, path=Path.Combine(dir, bit), name=bit;
      if(Directory.Exists(path)) { dir=path; name="__init__"; }
      foreach(string file in Directory.GetFiles(dir, name+".*"))
        if(Scripting.IsRegistered(Path.GetExtension(file)))
          return ModuleGenerator.Generate(Path.Combine(dir, file));
    }
    return null;
  }
  
  static readonly Hashtable builtinTypes = new Hashtable();
  static readonly ListDictionary builtinNamespaces = new ListDictionary();
}

} // namespace Scripting