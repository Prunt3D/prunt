pragma Ada_2022;

with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

procedure Prebuild is

   subtype String_Access is GNAT.OS_Lib.String_Access;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Index_Type => Positive, Element_Type => String);

   Working_Dir : constant String := Current_Directory;

   function Join_Path (Parts : String_Vectors.Vector) return String;
   procedure Run (Program : String; Args : String_Vectors.Vector; Dir : String := "");
   procedure Copy_Source_Files;
   procedure Build_Typescript;
   procedure Copy_Uplot;
   procedure Build_Ada_Resources;
   procedure Prepend_To_File (Path : String; Prefix : String);

   function Join_Path (Parts : String_Vectors.Vector) return String is
      Result : Unbounded_String;
   begin
      for Part of Parts loop
         if Length (Result) > 0 then
            Append (Result, Directory_Separator);
         end if;
         Append (Result, Part);
      end loop;
      return To_String (Result);
   end Join_Path;

   procedure Run (Program : String; Args : String_Vectors.Vector; Dir : String := "") is
      Prog_Path : constant String_Access := Locate_Exec_On_Path (Program);
      Old_Dir   : constant String := Current_Directory;
      Status    : Integer;
   begin
      if Prog_Path = null then
         raise Program_Error with "Cannot find on path: " & Program;
      end if;

      Put (Program);
      for A of Args loop
         Put (" " & A);
      end loop;
      New_Line;

      if Dir /= "" then
         Set_Directory (Dir);
      end if;
      Status := Spawn (Prog_Path.all, [for A of Args => new String'(A)]);
      if Dir /= "" then
         Set_Directory (Old_Dir);
      end if;
      if Status /= 0 then
         raise Program_Error with Program & " exited with status" & Status'Image;
      end if;
   end Run;

   procedure Copy_Source_Files is
      Src_Base  : constant String := Join_Path ([Working_Dir, "extras", "src"]);
      Dist_Base : constant String := Join_Path ([Working_Dir, "extras", "dist"]);
      Search    : Search_Type;
      Ent       : Directory_Entry_Type;
   begin
      Create_Path (Dist_Base);
      Start_Search
        (Search    => Search,
         Directory => Src_Base,
         Pattern   => "",
         Filter    => [Ordinary_File => True, others => False]);

      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Name : constant String := Simple_Name (Ent);
         begin
            if Name'Length < 3 or else Name (Name'Last - 2 .. Name'Last) /= ".ts" then
               Put_Line ("Copying extras/" & Name);
               Copy_File (Full_Name (Ent), Join_Path ([Dist_Base, Name]));
            end if;
         end;
      end loop;
      End_Search (Search);
   end Copy_Source_Files;

   procedure Build_Typescript is
   begin
      Run ("tsc", [], Dir => Join_Path ([Working_Dir, "extras"]));
   end Build_Typescript;

   procedure Copy_Uplot is
      Source_JS  : constant String := Join_Path ([Working_Dir, "..", "html", "uplot", "dist", "uPlot.esm.js"]);
      Target_JS  : constant String := Join_Path ([Working_Dir, "extras", "dist", "uPlot.esm.js"]);
      Source_CSS : constant String := Join_Path ([Working_Dir, "..", "html", "uplot", "dist", "uPlot.min.css"]);
      Target_CSS : constant String := Join_Path ([Working_Dir, "extras", "dist", "uPlot.min.css"]);
   begin
      if not Exists (Source_JS) or else not Exists (Source_CSS) then
         Run ("git", ["-C", "..", "submodule", "update", "--init", "html/uplot"]);
      end if;
      if not Exists (Source_JS) then
         raise Program_Error with "Cannot find uPlot.esm.js at " & Source_JS;
      end if;
      if not Exists (Source_CSS) then
         raise Program_Error with "Cannot find uPlot.min.css at " & Source_CSS;
      end if;

      Put_Line ("Copying uPlot.esm.js");
      Copy_File (Source_JS, Target_JS);
      Put_Line ("Copying uPlot.min.css");
      Copy_File (Source_CSS, Target_CSS);
   end Copy_Uplot;

   procedure Prepend_To_File (Path : String; Prefix : String) is
      Content : Unbounded_String;
      File    : File_Type;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         Append (Content, Get_Line (File) & ASCII.LF);
      end loop;
      Close (File);

      Create (File, Out_File, Path);
      Put (File, Prefix);
      Put (File, To_String (Content));
      Close (File);
   end Prepend_To_File;

   procedure Build_Ada_Resources is
      Target   : constant String := Join_Path ([Working_Dir, "generated_src_extras", "prunt_simulator_extra_resources.adb"]);
      Pragma_1 : constant String :=
        "pragma Warnings (Off, ""array aggregate using () is an obsolescent syntax, use [] instead"");" & ASCII.LF;
      Pragma_2 : constant String :=
        "pragma Warnings (Off, ""subprogram body """"Get_Content"""" not in alphabetical order"");" & ASCII.LF;
      Pragma_3 : constant String := "pragma Style_Checks (Off);" & ASCII.LF;
   begin
      Create_Path (Join_Path ([Working_Dir, "generated_src_extras"]));
      Run
        ("are",
         ["--lang=Ada",
          "-o",
          "generated_src_extras",
          "--content-only",
          "--name-access",
          "--rule=" & Join_Path (["extras", "package.xml"]),
          Join_Path (["extras", "dist"])],
         Dir => Working_Dir);
      Prepend_To_File (Target, Pragma_1 & Pragma_2 & Pragma_3);
   end Build_Ada_Resources;

begin
   Copy_Source_Files;
   Build_Typescript;
   Copy_Uplot;
   Build_Ada_Resources;
end Prebuild;
