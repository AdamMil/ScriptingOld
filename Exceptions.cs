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

namespace Scripting
{

public class AssertionFailedException : ScriptException
{ public AssertionFailedException(string message) : base(message) { }
}

public class AttributeException : ScriptException
{ public AttributeException(string message) : base(message) { }
  public AttributeException(object obj, string attr, string message) : base(message) { Object=obj; Attribute=attr; }
  public readonly object Object;
  public readonly string Attribute;
}

public class CompileTimeException : ScriptException
{ public CompileTimeException(string message) : base(message) { }
}

public class ScriptException : ApplicationException
{ public ScriptException() { }
  public ScriptException(string message) : base(message) { }
}

public class SyntaxErrorException : CompileTimeException
{ public SyntaxErrorException(string message) : base(message) { }
}

public class UndefinedVariableException : ScriptException
{ public UndefinedVariableException(string message) : base(message) { }

  public static UndefinedVariableException FromName(string varName)
  { return new UndefinedVariableException("use of unbound variable: "+varName);
  }
}

} // namespace Scripting
