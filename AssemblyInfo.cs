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

using System.Reflection;
using System.Runtime.CompilerServices;

[assembly: AssemblyTitle("Scripting Runtime")]
[assembly: AssemblyDescription("A platform for building dynamic languages that target the .NET runtime.")]
#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#else
[assembly: AssemblyConfiguration("Release")]
#endif
[assembly: AssemblyProduct("Scripting Runtime")]
[assembly: AssemblyCopyright("Copyright 2005, Adam Milazzo")]

[assembly: AssemblyVersion("0.5.0.0")]

[assembly: AssemblyKeyFile("../../Scripting.snk")]