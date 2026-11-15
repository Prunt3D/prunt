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

private with Ada.Containers.Indefinite_Vectors;

package Dimension_Check.Reports is

   type Severity is (Error, Warning, Note);
   --  Errors and warnings are findings about the checked code. Notes report code that could not be checked.

   type Finding (File_Length, Message_Length : Natural) is record
      Kind    : Severity;
      Line    : Positive;
      Column  : Positive;
      File    : String (1 .. File_Length);
      Message : String (1 .. Message_Length);
   end record;

   procedure Add (Item : Finding);
   --  Record Item. Safe to call from several jobs at once. A finding equal to one already recorded is dropped.

   procedure Print_All (Show_Notes : Boolean);
   --  Print every recorded finding to standard output, sorted by file, line and column, as
   --  "file:line:column: severity: message". Notes are printed only when Show_Notes is True.

   function Count (Kind : Severity) return Natural;
   --  Return the number of recorded findings of the given severity.

private

   package Finding_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Finding);

   function Before (Left, Right : Finding) return Boolean;
   --  Ordering used by Print_All: file, line, column, severity, message.

end Dimension_Check.Reports;
