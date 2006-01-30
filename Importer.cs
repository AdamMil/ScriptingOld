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
using Scripting.Backend;

namespace Scripting
{

public static class Importer
{ static Importer() { SearchPaths.Add("."); }

  public static void Import(TopLevel top, Dictionary<string,object> dict, MemberContainer from,
                            MemberContainer.ExportInfo ei, string[] names, string[] asNames, string myName)
  { if(names==null)
      foreach(KeyValuePair<string,object> de in dict)
      { string name = ei==null ? de.Key : ei.GetExternalName(de.Key);
        if(name!=null) top.Globals.Bind(name, de.Value, from);
      }
    else
    { if(asNames==null) asNames = names;
      for(int i=0; i<names.Length; i++)
      { object obj;
        string name = ei==null ? names[i] : ei.GetInternalName(names[i]);
        if(!dict.TryGetValue(name, out obj))
          throw new ArgumentException(myName+" does not contain a member called '"+names[i]+
                                      (name!=names[i] ? "' (renamed from '"+names[i]+"')" : "'"));
        top.Globals.Set(asNames[i], obj, from);
      }
    }
  }

  public static MemberContainer Load(string name) { return Load(name, true, false); }
  public static MemberContainer Load(string name, bool throwOnError) { return Load(name, throwOnError, false); }
  public static MemberContainer Load(string name, bool throwOnError, bool returnTop)
  { MemberContainer module, top;
    bool returnNow = false;

    string[] bits = name.Split('.');

    lock(LoadedModules)
    { if(!LoadedModules.TryGetValue(bits[0], out top))
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
  { MemberContainer module;
    if(!builtinTypes.TryGetValue(type, out module)) builtinTypes[type] = module = ModuleGenerator.Generate(type);
    return module;
  }

  public static readonly List<string> SearchPaths = new List<string>();
  public static readonly Dictionary<string,MemberContainer> LoadedModules = new Dictionary<string,MemberContainer>();

  static MemberContainer LoadBuiltin(string name)
  { string ns = Ops.GetCurrentLanguage().BuiltinsNamespace;
    if(ns==null) return null;

    SortedList<string,Type> dict;
    lock(builtinNamespaces)
      if(!builtinNamespaces.TryGetValue(ns, out dict))
      { builtinNamespaces[ns] = dict = new SortedList<string,Type>(4);
        foreach(Type type in Ops.GetCurrentLanguage().GetType().Assembly.GetTypes())
          if(type.IsPublic && type.Namespace==ns)
          { object[] attrs = type.GetCustomAttributes(typeof(ScriptNameAttribute), false);
            if(attrs.Length!=0) dict[((ScriptNameAttribute)attrs[0]).Name] = type;
          }
      }

    { Type type;
      if(!dict.TryGetValue(name, out type))
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

  static readonly Dictionary<Type,MemberContainer> builtinTypes = new Dictionary<Type,MemberContainer>();
  static readonly SortedList<string,SortedList<string,Type>> builtinNamespaces = new SortedList<string,SortedList<string,Type>>();
}

} // namespace Scripting