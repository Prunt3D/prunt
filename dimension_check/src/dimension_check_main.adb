--  Part of the Prunt Motion Controller
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  dimension_check implements the dimensionality checking of GNAT and is derived from it, in particular from
--  gcc/ada/sem_dim.adb, Copyright (C) 2011-2025, Free Software Foundation, Inc.
--
--  This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
--  License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any
--  later version.
--
--  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
--  details.
--
--  You should have received a copy of the GNU General Public License along with this program, in the file COPYING. If
--  not, see <https://www.gnu.org/licenses/>.
--------------------------------------------------

with Ada.Command_Line;
with Ada.Strings;              use Ada.Strings;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Text_IO;              use Ada.Text_IO;
with Dimension_Check.Analysis; use Dimension_Check.Analysis;
with Dimension_Check.Reports;  use Dimension_Check.Reports;
with GNATCOLL.Opt_Parse;
with Libadalang.Analysis;      use Libadalang.Analysis;
with Libadalang.Helpers;       use Libadalang.Helpers;

procedure Dimension_Check_Main is

   type Checker_Access is access Checker;

   Checkers : array (Job_ID range 1 .. 512) of Checker_Access;

   Extras : Extra_Check_Set := [others => True];

   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array);
   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);
   procedure App_Post_Process (Context : App_Context; Jobs : App_Job_Context_Array);

   package App is new
     Libadalang.Helpers.App
       (Name               => "dimension_check",
        Description        =>
          "Checks the physical dimensions of Ada code that declares them with Annotate aspects. Exits with a failure"
          & " status if any error is found, or any warning with --warnings-as-errors.",
        Enable_Parallelism => True,
        App_Setup          => App_Setup,
        Process_Unit       => Process_Unit,
        App_Post_Process   => App_Post_Process);

   package GNAT_Compatible is new
     GNATCOLL.Opt_Parse.Parse_Flag
       (Parser => App.Args.Parser,
        Long   => "--gnat-compatible",
        Help   =>
          "Make only the checks that GNAT's Dimension_System aspect makes, and treat a dimensionless subtype like the"
          & " root type of its system.");

   package Verbose is new
     GNATCOLL.Opt_Parse.Parse_Flag
       (Parser => App.Args.Parser,
        Short  => "-v",
        Long   => "--verbose",
        Help   => "Also list the constructs that could not be checked because Libadalang could not resolve them.");

   package Warnings_As_Errors is new
     GNATCOLL.Opt_Parse.Parse_Flag
       (Parser => App.Args.Parser,
        Short  => "-W",
        Long   => "--warnings-as-errors",
        Help   => "Exit with a failure status if any warning is found.");

   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array) is
      pragma Unreferenced (Context);
   begin
      if Jobs'Last > Checkers'Last then
         Abort_App ("at most" & Checkers'Last'Image & " jobs are supported");
      end if;

      for J in Jobs'Range loop
         Checkers (J) := new Checker;
      end loop;

      if GNAT_Compatible.Get then
         Extras := [others => False];
      end if;
   end App_Setup;

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
   begin
      Check_Unit (Checkers (Context.ID).all, Unit, Extras);
   end Process_Unit;

   procedure App_Post_Process (Context : App_Context; Jobs : App_Job_Context_Array) is
      pragma Unreferenced (Context, Jobs);

      Errors     : constant Natural := Dimension_Check.Reports.Count (Error);
      Warnings   : constant Natural := Dimension_Check.Reports.Count (Warning);
      Unresolved : constant Natural := Dimension_Check.Reports.Count (Note);
   begin
      Print_All (Show_Notes => Verbose.Get);

      if Errors > 0 or else Warnings > 0 then
         Put_Line
           (Standard_Error,
            Trim (Errors'Image, Both) & " dimension error(s), " & Trim (Warnings'Image, Both) & " warning(s).");
      end if;

      if Unresolved > 0 and then not Verbose.Get then
         Put_Line
           (Standard_Error,
            Trim (Unresolved'Image, Both)
            & " construct(s) not checked because Libadalang could not resolve them; use -v to list them.");
      end if;

      if Errors > 0 or else (Warnings > 0 and then Warnings_As_Errors.Get) then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end App_Post_Process;

begin
   App.Run;
end Dimension_Check_Main;
