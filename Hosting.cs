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
using System.Text.RegularExpressions;
using Scripting.Backend;

namespace Scripting
{

#region Scripting
public sealed class Scripting
{ Scripting() { }

  static Scripting()
  { try 
    { // a cleaner design would be to simply load all the DLLs and enumerate/register their languages.
      // the problem is that loading a bunch of possibly-unneeded assemblies at startup is slow.
      string langPath=Path.GetFullPath(LanguagesPath), path=Path.Combine(langPath, "config.txt");
      if(File.Exists(path))
      { StreamReader sr = new StreamReader(path);
        Regex langre = new Regex(@"^\s*(?<!#)(\S+)\s+(\S+)\s+(.*)", RegexOptions.Singleline);
        while(true)
        { string line = sr.ReadLine();
          if(line==null) break;
          Match m = langre.Match(line);
          if(m.Success)
          { string type=m.Groups[2].Value, fullPath=m.Groups[3].Value;
            if(string.Compare(fullPath, 0, "GAC:", 0, 4, true)!=0) fullPath = Path.Combine(langPath, fullPath);
            fullPath = type+" "+fullPath;
            foreach(string ext in m.Groups[1].Value.Split(';')) extensions[NormalizeExtension(ext)] = fullPath;
          }
        }
        sr.Close();
      }
    }
    catch { }
  }

  public static string DllCache { get { return Path.Combine(InstallationPath, "dllcache"); } }

  public static string InstallationPath
  { get { return Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location); }
  }

  public static string LanguagesPath { get { return Path.Combine(InstallationPath, "languages"); } }

  public static bool IsRegistered(string extension) { return extensions.ContainsKey(NormalizeExtension(extension)); }

  public static Language LoadLanguage(string extension)
  { if(extension==null) throw new ArgumentNullException();

    extension = NormalizeExtension(extension);
    string path;
    lock(extensions)
      if(!extensions.TryGetValue(extension, out path))
        throw new ArgumentException("No language is registered to handle files of type "+extension);

    Language lang;
    lock(languages)
      if(!languages.TryGetValue(path, out lang))
      { int index = path.IndexOf(' ');
        if(index==-1) throw new FormatException("Invalid language path "+path);
        string typeName=path.Substring(0, index), assPath=path.Substring(index+1);

        Assembly ass = string.Compare(assPath, 0, "GAC:", 0, 4, true)!=0
                         ? Assembly.LoadFrom(assPath) : Assembly.LoadWithPartialName(assPath.Substring(4));
        if(ass==null) throw new TypeLoadException("Unable to load language: "+path);
        Type type = ass.GetType(typeName, true);
        FieldInfo fi = type.GetField("Instance", BindingFlags.Static|BindingFlags.Public);
        if(fi!=null && fi.FieldType.IsSubclassOf(typeof(Language))) lang = (Language)fi.GetValue(null);
        if(lang==null) throw new TypeLoadException("Unable to load language: "+path);
        languages[path] = lang;
      }

    return lang;
  }

  public static void RegisterLanguage(string extensions, string assemblyPath)
  { RegisterLanguage(extensions, assemblyPath, null);
  }

  public static void RegisterLanguage(string extensions, string assemblyPath, Language language)
  { if(extensions==null || assemblyPath==null) throw new ArgumentNullException();

    if(language!=null)
      lock(languages)
      { Language lang;
        if(languages.TryGetValue(assemblyPath, out lang) && lang!=language)
          throw new InvalidOperationException("Assembly '"+assemblyPath+"' is already registered to language "+lang.Name);
        languages[assemblyPath] = language;
      }

    lock(Scripting.extensions)
      foreach(string ext in extensions.Split(';')) // TODO: warn about overwriting extensions that are already registered
        Scripting.extensions[NormalizeExtension(ext)] = assemblyPath;
  }

  static string NormalizeExtension(string ext)
  { if(ext.StartsWith(".")) ext = ext.Substring(1);
    return ext.Trim().ToLower();
  }

  static readonly SortedList<string,string> extensions = new SortedList<string,string>();
  static readonly SortedList<string,Language> languages = new SortedList<string,Language>();
}
#endregion

#region TextFrontend
public abstract class TextFrontend
{ protected TextFrontend(TextReader input, TextWriter output) { Input=input; Output=output; Banner=Compiled=true; }

  protected void CompileFile(string filename, string outfile, PEFileKinds exeType)
  { bool stdin = filename=="-";
    string basename = stdin ? "main" : Path.GetFileNameWithoutExtension(filename);

    if(outfile!=null) Output.WriteLine("Parsing {0}...", stdin ? "standard input" : filename);
    Node node;
    if(stdin) node = Options.Current.Language.Parse("<stdin>", Input);
    else
    { // TODO: implement this. sys.path[0] = Path.GetDirectoryName(Path.GetFullPath(filename));
      node = Options.Current.Language.ParseFile(filename);
    }

    if(outfile!=null)
    { Output.WriteLine("Compiling...");
      ModuleGenerator.Generate(basename, basename, outfile, AST.CreateCompiled(AST.Create(node)),
                               exeType); // FIXME: if snippetmaker is used (eg, if eval is called), the snippets will be lost
      Output.WriteLine("Successfully wrote "+outfile);
    }
    else
      try
      { TopLevel.Current = new TopLevel();
        MemberContainer builtins = Options.Current.Language.Builtins;
        if(builtins!=null) builtins.Import(TopLevel.Current);
        SnippetMaker.Generate(AST.CreateCompiled(AST.Create(node)), basename).Run(null);
      }
      finally { if(WriteSnippets) SnippetMaker.DumpAssembly(); }
  }

  protected virtual void DisplayReturn(object value) { Output.WriteLine("=> "+Ops.ToCode(value)); }

  protected void DoInteractive()
  { TopLevel.Current = new TopLevel();
    MemberContainer builtins = Options.Current.Language.Builtins;
    if(builtins!=null) builtins.Import(TopLevel.Current);

    while(true)
    { string code = GetSnippet();
      if(code==null) break;
      try
      { Node node = Options.Current.Language.Parse("<interactive>", code);
        DisplayReturn(Compiled ? SnippetMaker.Generate(AST.CreateCompiled(node)).Run(null)
                               : AST.Create(node).Evaluate());
      }
      catch(Exception e) { Output.WriteLine("ERROR: "+e.ToString()); }
    }
  }

  protected int DoMain(string[] args)
  { Options.Current.Debug = true;
    Options.Current.Optimize = OptimizeType.Speed;
    AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(UnhandledException);

    string filename=null, outfile=null;
    PEFileKinds exeType = PEFileKinds.ConsoleApplication;
    List<string> errors = new List<string>();

    for(int i=0; i<args.Length; i++)
    { string arg = args[i];
      if((arg[0]=='-' || arg[0]=='/') && arg.Length!=1 && arg!="--")
      { string value;
        int index = arg.IndexOfAny(new char[] { ':', '-', '+' }, 1);
        if(index==-1) { value=""; arg=arg.Substring(1); }
        else
        { value = arg[index]==':' ? arg.Substring(index+1) : arg[index].ToString();
          arg=arg.Substring(1, index-1);
        }

        try
        { switch(arg)
          { case "banner": Banner=IsTrue(value); break;
            case "compiled": Compiled=IsTrue(value); break;
            case "debug": Options.Current.Debug=IsTrue(value); break;
            case "?": case "help": case "-help": ShowUsage(); return 0;
            // TODO: implement this. case "lib": sys.path.insert(0, value); break;
            case "o": case "optimize":
              if(value=="speed") Options.Current.Optimize = OptimizeType.Speed;
              else if(value=="size") Options.Current.Optimize = OptimizeType.Size;
              else Options.Current.Optimize = value!="none" && IsTrue(value) ? OptimizeType.Speed : OptimizeType.None;
              break;
            case "out": outfile=value; break;
            case "snippets": WriteSnippets=IsTrue(value); break;
            case "t": case "target":
              switch(value)
              { case "exe": exeType = PEFileKinds.ConsoleApplication; break;
                case "winexe": exeType = PEFileKinds.WindowApplication; break;
                case "dll": case "lib": case "library":
                  exeType = PEFileKinds.Dll; break;
                default: errors.Add("Unknown value for -type: "+value); break;
              }
              break;
            default: if(!HandleSwitch(arg, value)) errors.Add("Unknown argument: "+arg); break;
          }
        }
        catch { errors.Add("Invalid value for -"+arg+": "+value); }
      }
      else if(arg=="/") errors.Add("Invalid switch: /");
      else
      { // TODO: implement this
        if(arg!="--") { /*sys.argv.append(arg);*/ filename=arg; }
        /*else sys.argv.append("");
        for(i++; i<args.Length; i++) sys.argv.append(args[i]);
        */
      }
    }

    if(errors.Count!=0)
    { foreach(string error in errors) Output.WriteLine("ERROR: "+error);
      ShowUsage();
      return 1;
    }
    errors = null;

    if(Banner && (filename==null || outfile!=null)) ShowBanner();

    if(filename==null)
      try { DoInteractive(); }
      finally { if(WriteSnippets) SnippetMaker.DumpAssembly(); }
    else
      try { CompileFile(filename, outfile, exeType); }
      catch(Exception e)
      { Output.WriteLine("Errors occurred during compilation:");
        if(e is SyntaxErrorException) Output.WriteLine(e.Message);
        else Output.WriteLine(e);
        return 1;
      }
    
    return 0;
  }

  protected static bool IsTrue(string value)
  { switch(value.ToLower())
    { case "-": case "0": case "no": case "off": case "false": return false;
      case "+": case "1": case "yes": case "on": case "true": return true;
      default: throw new ArgumentException();
    }
  }

  static string GenerateName(string name, PEFileKinds type)
  { string ext = type==PEFileKinds.Dll ? ".dll" : ".exe";
    string baseName = Path.GetDirectoryName(name);
    if(baseName!="") baseName += Path.DirectorySeparatorChar;
    baseName += Path.GetFileNameWithoutExtension(name);

    name = baseName + ext;
    for(int i=0; File.Exists(name); i++) name = baseName + i.ToString() + ext;
    return name;
  }

  protected abstract string GetSnippet();
  protected virtual bool HandleSwitch(string name, string value) { return false; }
  protected abstract void ShowBanner();
  protected abstract void ShowUsage();

  protected void ShowCompilationOptions()
  { Output.WriteLine("Compilation options:");
    Output.WriteLine("-banner[-|+]       Display copyright banner");
    Output.WriteLine("-compiled[-|+]     Enable compiled code (default=on)");
    Output.WriteLine("-debug[-|+]        Emit debugging information (default=on)");
    Output.WriteLine("-help              Show this message");
    Output.WriteLine("-lib:<path>        Specify additional library paths");
    Output.WriteLine("-o[ptimize][-|+]   Enable optimizations (default=on)");
    Output.WriteLine("-out:<file>        Compile and save the output (overrides -compiled)");
    Output.WriteLine("-snippets[-|+]     Save the snippets .dll");
    Output.WriteLine("-t[arget]:exe      Build a console application (used with -out)");
    Output.WriteLine("-t[arget]:library  Build a library (used with -out)");
    Output.WriteLine("-t[arget]:winexe   Build a windowed application (used with -out)");
  }

  protected TextReader Input;
  protected TextWriter Output;
  protected bool Banner, Compiled, WriteSnippets;

  void UnhandledException(object sender, UnhandledExceptionEventArgs e)
  { Output.WriteLine("An unhandled exception occurred{0}:",
                      e.IsTerminating ? " and the application must terminate" : "");
    Output.WriteLine(e.ExceptionObject);
  }
}
#endregion

} // namespace Scripting