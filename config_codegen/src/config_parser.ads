-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2026 Liam Powell (liam@prunt3d.com)            --
--                                                                         --
--  This program is free software: you can redistribute it and/or modify   --
--  it under the terms of the GNU General Public License as published by   --
--  the Free Software Foundation, either version 3 of the License, or      --
--  (at your option) any later version.                                    --
--                                                                         --
--  This program is distributed in the hope that it will be useful,        --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--  GNU General Public License for more details.                           --
--                                                                         --
--  You should have received a copy of the GNU General Public License      --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                         --
-----------------------------------------------------------------------------

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
