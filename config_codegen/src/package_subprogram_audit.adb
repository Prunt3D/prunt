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

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Text_IO;                use Ada.Text_IO;
with Libadalang.Analysis;        use Libadalang.Analysis;
with Libadalang.Common;          use Libadalang.Common;
with Libadalang.Doc_Utils;
with Libadalang.Helpers;

procedure Package_Subprogram_Audit is
   Finding_Count : Natural := 0;

   function Has_Suffix (Value, Suffix : String) return Boolean is
     (Value'Length >= Suffix'Length and then Value (Value'Last - Suffix'Length + 1 .. Value'Last) = Suffix);

   function Line_Image (Line : Natural) return String is
     (Trim (Line'Image, Both));

   function Display_Filename (Filename : String) return String is
      Current_Directory : constant String := Ada.Directories.Current_Directory;
   begin
      if Filename'Length > Current_Directory'Length
        and then Filename (Filename'First .. Filename'First + Current_Directory'Length - 1) = Current_Directory
        and then Filename (Filename'First + Current_Directory'Length) in '/' | '\'
      then
         return Filename (Filename'First + Current_Directory'Length + 1 .. Filename'Last);
      else
         return Filename;
      end if;
   end Display_Filename;

   function Subprogram_Name (Node : Ada_Node'Class) return String is
   begin
      if Node.Kind in Ada_Classic_Subp_Decl then
         return To_String (Node.As_Classic_Subp_Decl.F_Subp_Spec.P_Name.Text);
      elsif Node.Kind in Ada_Base_Subp_Body then
         return To_String (Node.As_Base_Subp_Body.F_Subp_Spec.P_Name.Text);
      elsif Node.Kind in Ada_Generic_Subp_Instantiation then
         return To_String (Node.As_Generic_Subp_Instantiation.F_Subp_Name.Text);
      else
         raise Program_Error with "Unexpected subprogram node: " & Node.Kind_Name;
      end if;
   end Subprogram_Name;

   procedure Report (Unit : Analysis_Unit; Node : Ada_Node'Class; Message : String) is
   begin
      Finding_Count := Finding_Count + 1;
      Put_Line
        (Display_Filename (Unit.Get_Filename)
         & ":"
         & Line_Image (Natural (Node.Sloc_Range.Start_Line))
         & ": "
         & Message
         & ": "
         & Subprogram_Name (Node));
   end Report;

   procedure Audit_Spec_Part (Unit : Analysis_Unit; Part : Declarative_Part'Class) is
   begin
      if Part.Is_Null then
         return;
      end if;

      for Node of Part.F_Decls loop
         if Node.Kind in Ada_Classic_Subp_Decl | Ada_Base_Subp_Body | Ada_Generic_Subp_Instantiation
           and then Libadalang.Doc_Utils.Get_Documentation (Node.As_Basic_Decl).Doc.Length = 0
         then
            Report (Unit, Node, "package-level subprogram has no documentation");
         end if;
      end loop;
   end Audit_Spec_Part;

   procedure Audit_Package_Declaration (Unit : Analysis_Unit; Declaration : Base_Package_Decl'Class) is
   begin
      Audit_Spec_Part (Unit, Declaration.F_Public_Part);
      Audit_Spec_Part (Unit, Declaration.F_Private_Part);
   end Audit_Package_Declaration;

   procedure Process_Unit (Context : Libadalang.Helpers.App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);

      Filename : constant String := Unit.Get_Filename;
      Root     : constant Ada_Node := Unit.Root;
      Item     : Basic_Decl;
   begin
      if Root.Is_Null or else Root.Kind not in Ada_Compilation_Unit then
         return;
      end if;

      if Root.As_Compilation_Unit.F_Body.Kind not in Ada_Library_Item then
         return;
      end if;
      Item := Root.As_Compilation_Unit.F_Body.As_Library_Item.F_Item;

      if Has_Suffix (Filename, ".adb") and then Item.Kind in Ada_Package_Body then
         for Node of Item.As_Package_Body.F_Decls.F_Decls loop
            if Node.Kind in Ada_Subp_Decl | Ada_Generic_Subp_Instantiation then
               Report (Unit, Node, "subprogram declaration is in a package body");
            end if;
         end loop;
      elsif Has_Suffix (Filename, ".ads") then
         if Item.Kind in Ada_Package_Decl then
            Audit_Package_Declaration (Unit, Item.As_Package_Decl);
         elsif Item.Kind in Ada_Generic_Package_Decl then
            Audit_Package_Declaration (Unit, Item.As_Generic_Package_Decl.F_Package_Decl);
         end if;
      end if;
   end Process_Unit;

   package App is new
     Libadalang.Helpers.App
       (Name         => "package_subprogram_audit",
        Description  => "Audits package-level Ada subprogram declarations and documentation.",
        Process_Unit => Process_Unit);
begin
   App.Run;

   if Finding_Count > 0 then
      Put_Line
        (Standard_Error,
         Line_Image (Finding_Count) & " package-level subprogram audit finding(s).");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Package_Subprogram_Audit;
