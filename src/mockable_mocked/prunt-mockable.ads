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

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;

package Prunt.Mockable is
private

   package File_Content_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   use type File_Content_Vectors.Vector;
   package File_Maps is new Ada.Containers.Indefinite_Ordered_Maps (String, File_Content_Vectors.Vector);

   protected Filesystem is
      procedure Create (Name : String);
      procedure Write_Line (Name : String; Line : String);
      function Read_Line (Name : String; Index : Positive) return String;
      function Line_Count (Name : String) return Natural;
      function Exists (Name : String) return Boolean;
      procedure Delete (Name : String);
      procedure Rename (Old_Name, New_Name : String);
   private
      Store : File_Maps.Map;
   end Filesystem;

end Prunt.Mockable;
