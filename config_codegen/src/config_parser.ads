--  Part of the Prunt Motion Controller
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
--  documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
--  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice (including the next paragraph) shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
--  THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.
--------------------------------------------------

pragma Extensions_Allowed (On);

with Config_Types;        use Config_Types;
with Libadalang.Analysis; use Libadalang.Analysis;
with VSS.Strings;         use VSS.Strings;
with Libadalang.Common;   use Libadalang.Common;

package Config_Parser is

   function Parse (Context : Libadalang.Analysis.Analysis_Context; Filename : String) return Module_Data;

private

   procedure Raise_Error (Node : Ada_Node'Class; Message : String);
   --  Raise a Constraint_Error along with details of the provided node, including the source code location.

   function Has_Prunt_Config_Aspect (Decl : Base_Type_Decl; Recursive : Boolean := True) return Boolean;
   --  Returns True if the type declaration contains any Prunt_Config annotation aspects.

   function Get_Comments_Starting_After (Start_Token : Token_Reference) return Virtual_String;
   --  Returns the contents of the comment block directly after the given token, ignoring any whitespace.
   --
   --  Lines with nothing except whitespace after -- are replaced with LF. Other lines are joined using a space
   --  character after trimming whitespace.

   function Get_Range (Decl : Base_Type_Decl) return Range_Spec;
   --  Recursively gets the parent of the given type declaration and returns the range spec if any of the resolved
   --  types contain an explicit range constraint, including the initial type. If no range spec is encountered then a
   --  null node is returned.
   --
   --  Raises an exception if the type does not resolve to a Type_Decl or Subtype_Decl at any point during the search.

end Config_Parser;
