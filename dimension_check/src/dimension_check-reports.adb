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

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

package body Dimension_Check.Reports is

   function Before (Left, Right : Finding) return Boolean is
   begin
      if Left.File /= Right.File then
         return Left.File < Right.File;
      elsif Left.Line /= Right.Line then
         return Left.Line < Right.Line;
      elsif Left.Column /= Right.Column then
         return Left.Column < Right.Column;
      elsif Left.Kind /= Right.Kind then
         return Left.Kind < Right.Kind;
      else
         return Left.Message < Right.Message;
      end if;
   end Before;

   package Sorting is new Finding_Vectors.Generic_Sorting ("<" => Before);

   protected Store is
      procedure Add (Item : Finding);
      procedure Sort;
      function Get return Finding_Vectors.Vector;
      function Count (Kind : Severity) return Natural;
   private
      Items : Finding_Vectors.Vector;
   end Store;

   protected body Store is
      procedure Add (Item : Finding) is
      begin
         if not Items.Contains (Item) then
            Items.Append (Item);
         end if;
      end Add;

      procedure Sort is
      begin
         Sorting.Sort (Items);
      end Sort;

      function Get return Finding_Vectors.Vector
      is (Items);

      function Count (Kind : Severity) return Natural is
         Result : Natural := 0;
      begin
         for Item of Items loop
            if Item.Kind = Kind then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Count;
   end Store;

   procedure Add (Item : Finding) is
   begin
      Store.Add (Item);
   end Add;

   procedure Print_All (Show_Notes : Boolean) is
   begin
      Store.Sort;
      for Item of Store.Get loop
         if Item.Kind /= Note or else Show_Notes then
            Put_Line
              (Item.File
               & ":"
               & Trim (Item.Line'Image, Both)
               & ":"
               & Trim (Item.Column'Image, Both)
               & ": "
               & (case Item.Kind is
                    when Error   => "error",
                    when Warning => "warning",
                    when Note    => "note")
               & ": "
               & Item.Message);
         end if;
      end loop;
   end Print_All;

   function Count (Kind : Severity) return Natural
   is (Store.Count (Kind));

end Dimension_Check.Reports;
