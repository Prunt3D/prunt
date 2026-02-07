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

--  This was too unwieldy for a makefile since it does not really follow makefile rules.

pragma Ada_2022;

with Ada.Calendar;          use Ada.Calendar;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;       use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.Expect;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

procedure Prebuild is

   subtype String_Access is GNAT.OS_Lib.String_Access;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Index_Type => Positive, Element_Type => String);

   Working_Dir : constant String := Current_Directory;

   function Join_Path (Parts : String_Vectors.Vector) return String;
   --  Join path parts with the OS path separator.

   function Needs_Rebuild (Target : String; Sources : String_Vectors.Vector) return Boolean;
   --  True if `Target` does not exist or any source in `Sources` is newer than it. The prebuild binary is always
   --  included.

   procedure Run (Program : String; Args : String_Vectors.Vector; Dir : String := "");
   --  Run Program with `Args`, optionally changing to `Dir` first.

   procedure Collect_Files (Dir : String; Result : in out String_Vectors.Vector; Recursive : Boolean := False);
   --  Append all file paths under `Dir` to `Result`.

   procedure Collect_Directories (Dir : String; Result : in out String_Vectors.Vector; Recursive : Boolean := False);
   --  Append all directory paths under `Dir` to `Result`.

   procedure Collect_Files_Containing
     (Dir : String; Needle : String; Result : in out String_Vectors.Vector; Recursive : Boolean := False);
   --  Append all file paths under `Dir` that contain `Needle` to `Result`.

   procedure Prepend_To_File (Path : String; Prefix : String);
   --  Prepend `Prefix` to the file at `Path`.

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

   function Needs_Rebuild (Target : String; Sources : String_Vectors.Vector) return Boolean is
   begin
      if not Exists (Target) then
         Put_Line ("Rebuilding because target does not exist: " & Target'Image);
         return True;
      end if;

      for S of Sources loop
         if not Exists (S) then
            raise Constraint_Error with "Source not found: " & S;
         end if;

         if Modification_Time (S) > Modification_Time (Target) then
            Put_Line ("Rebuilding target " & Target'Image & " because source is newer: " & S'Image);
            return True;
         end if;
      end loop;

      if Modification_Time (Join_Path ([Working_Dir, "prebuild"])) > Modification_Time (Target) then
         Put_Line ("Rebuilding target " & Target'Image & " because prebuild binary is newer.");
         return True;
      end if;

      return False;
   end Needs_Rebuild;

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
      Put_Line ("");

      if Dir /= "" then
         Set_Directory (Dir);
      end if;
      Status := Spawn (Prog_Path.all, [for A of Args => new String'(A)]);
      --  We could avoid allocation and memory leaks here, but it really does not matter.
      if Dir /= "" then
         Set_Directory (Old_Dir);
      end if;
      if Status /= 0 then
         raise Program_Error with Program & " exited with status" & Status'Image;
      end if;
   end Run;

   procedure Collect_Files (Dir : String; Result : in out String_Vectors.Vector; Recursive : Boolean := False) is
      Search : Search_Type;
      Ent    : Directory_Entry_Type;
   begin
      Start_Search
        (Search    => Search,
         Directory => Dir,
         Pattern   => "",
         Filter    => [Ordinary_File => True, Directory => Recursive, others => False]);

      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         case Kind (Ent) is
            when Directory     =>
               if Simple_Name (Ent) not in "." | ".." then
                  Collect_Files (Full_Name (Ent), Result, Recursive);
               --  TODO: Does GNAT have some kind of built-in traversal which avoids . and ..?

               end if;

            when Ordinary_File =>
               Result.Append (Full_Name (Ent));

            when others        =>
               null;
         end case;
      end loop;
      End_Search (Search);
   end Collect_Files;

   procedure Collect_Directories (Dir : String; Result : in out String_Vectors.Vector; Recursive : Boolean := False) is
      Search : Search_Type;
      Ent    : Directory_Entry_Type;
   begin
      Start_Search
        (Search    => Search,
         Directory => Dir,
         Pattern   => "",
         Filter    => [Ordinary_File => False, Directory => Recursive, others => False]);

      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         if Kind (Ent) = Directory and then Simple_Name (Ent) not in "." | ".." then
            Result.Append (Full_Name (Ent));
            if Recursive then
               Collect_Directories (Full_Name (Ent), Result, Recursive);
            end if;
         end if;
      end loop;
      End_Search (Search);
   end Collect_Directories;

   procedure Collect_Files_Containing
     (Dir : String; Needle : String; Result : in out String_Vectors.Vector; Recursive : Boolean := False)
   is
      All_Files : String_Vectors.Vector;
   begin
      Collect_Files (Dir, All_Files, Recursive => Recursive);

      for F of All_Files loop
         declare
            File  : File_Type;
            Found : Boolean := False;
         begin
            Open (File, In_File, F);
            while not End_Of_File (File) and then not Found loop
               if Index (Get_Line (File), Needle) /= 0 then
                  Found := True;
               end if;
            end loop;
            Close (File);

            if Found then
               Result.Append (F);
            end if;
         end;
      end loop;
   end Collect_Files_Containing;

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

   procedure Ensure_Submodules is
      Uplot_Dist   : constant String := Join_Path ([Working_Dir, "html", "uplot", "dist"]);
      Uplot_ESM_JS : constant String := Join_Path ([Uplot_Dist, "uPlot.esm.js"]);
      Uplot_Types  : constant String := Join_Path ([Uplot_Dist, "uPlot.d.ts"]);
   begin
      Run ("git", ["submodule", "update", "--init", Join_Path (["html", "uplot"])]);
   exception
      when Program_Error =>
         if Exists (Uplot_ESM_JS) and then Exists (Uplot_Types) then
            Put_Line ("Ignoring html/uplot submodule update failure because build inputs are already present.");
         else
            raise;
         end if;
   end Ensure_Submodules;

   procedure Build_Html_Dist is
      Src_Base  : constant String := Join_Path ([Working_Dir, "html", "src"]);
      Dist_Base : constant String := Join_Path ([Working_Dir, "html", "dist"]);
      Src_Dirs  : String_Vectors.Vector;
      Src_Files : String_Vectors.Vector;
      Ts_Files  : String_Vectors.Vector;
   begin
      if not Exists (Dist_Base) then
         Create_Directory (Dist_Base);
      end if;

      Collect_Directories (Src_Base, Src_Dirs, Recursive => True);
      Collect_Files (Src_Base, Src_Files, Recursive => True);

      for S of Src_Dirs loop
         declare
            Rel  : constant String :=
              S (S'First + Src_Base'Length + String'("" & Directory_Separator)'Length .. S'Last);
            Dest : constant String := Join_Path ([Dist_Base, Rel]);
         begin
            if not Exists (Dest) then
               Put_Line ("Creating directory " & Rel);
               Create_Path (Dest);
            end if;
         end;
      end loop;

      for S of Src_Files loop
         declare
            Rel  : constant String :=
              S (S'First + Src_Base'Length + String'("" & Directory_Separator)'Length .. S'Last);
            Dest : constant String := Join_Path ([Dist_Base, Rel]);
         begin
            if not Exists (Dest) or else Modification_Time (S) > Modification_Time (Dest) then
               Create_Path (Containing_Directory (Dest));
               Put_Line ("Copying " & Rel);
               Copy_File (S, Dest);
            end if;
            if Rel'Length >= 3 and then Rel (Rel'Last - 2 .. Rel'Last) = ".ts" then
               Ts_Files.Append (S);
            end if;
         end;
      end loop;

      Ts_Files.Append (Join_Path ([Working_Dir, "html", "tsconfig.json"]));
      Ts_Files.Append (Join_Path ([Working_Dir, "html", "uplot", "dist", "uPlot.d.ts"]));

      if Needs_Rebuild (Join_Path ([Dist_Base, "main.js"]), Ts_Files) then
         Run ("tsc", [], Dir => Join_Path ([Working_Dir, "html"]));
      end if;

      if not Exists (Join_Path ([Dist_Base, "uPlot.esm.js"]))
        or else
          Modification_Time (Join_Path ([Working_Dir, "html", "uplot", "dist", "uPlot.esm.js"]))
          > Modification_Time (Join_Path ([Dist_Base, "uPlot.esm.js"]))
      then
         Put_Line ("Copying uPlot.esm.js");
         Copy_File
           (Join_Path ([Working_Dir, "html", "uplot", "dist", "uPlot.esm.js"]),
            Join_Path ([Dist_Base, "uPlot.esm.js"]));
      end if;
   end Build_Html_Dist;

   procedure Clean_Dist_Stale is
      Src_Base   : constant String := Join_Path ([Working_Dir, "html", "src"]);
      Dist_Base  : constant String := Join_Path ([Working_Dir, "html", "dist"]);
      Dist_Files : String_Vectors.Vector;
      Removed    : Boolean := False;
   begin
      --  We do not clean up stale directories as ARE does not pick them up anyway.

      if not Exists (Dist_Base) then
         return;
      end if;

      Collect_Files (Dist_Base, Dist_Files, Recursive => True);

      for D of Dist_Files loop
         declare
            Rel  : constant String :=
              D (D'First + Dist_Base'Length + String'("" & Directory_Separator)'Length .. D'Last);
            Keep : Boolean := False;
         begin
            if Exists (Join_Path ([Src_Base, Rel])) then
               Keep := True;
            end if;

            if Rel'Length >= 3
              and then Rel (Rel'Last - 2 .. Rel'Last) = ".js"
              and then Exists (Join_Path ([Src_Base, Rel (Rel'First .. Rel'Last - 3) & ".ts"]))
            then
               Keep := True;
            end if;

            if Rel'Length >= 7
              and then Rel (Rel'Last - 6 .. Rel'Last) = ".js.map"
              and then Exists (Join_Path ([Src_Base, Rel (Rel'First .. Rel'Last - 7) & ".ts"]))
            then
               Keep := True;
            end if;

            if Rel = "uPlot.esm.js" then
               Keep := True;
            end if;

            if not Keep then
               Delete_File (Join_Path ([Dist_Base, D]));
               Removed := True;
            end if;
         end;
      end loop;

      if Removed then
         declare
            Ada_Resources : constant String :=
              Join_Path ([Working_Dir, "generated_src_html", "prunt-web_server_resources.adb"]);
         begin
            if Exists (Ada_Resources) then
               Delete_File (Ada_Resources);
            end if;
         end;
      end if;
   end Clean_Dist_Stale;

   procedure Build_Ada_Resources is
      Target   : constant String := Join_Path ([Working_Dir, "generated_src_html", "prunt-web_server_resources.adb"]);
      Sources  : String_Vectors.Vector;
      Pragma_1 : constant String :=
        "pragma Warnings (Off, ""array aggregate using () is an obsolescent syntax, use [] instead"");" & ASCII.LF;
      Pragma_2 : constant String :=
        "pragma Warnings (Off, ""subprogram body """"Get_Content"""" not in alphabetical order"");" & ASCII.LF;
      Pragma_3 : constant String := "pragma Style_Checks (Off);" & ASCII.LF;
   begin
      Collect_Files (Join_Path ([Working_Dir, "html", "dist"]), Sources, Recursive => True);
      Sources.Append (Join_Path ([Working_Dir, "html", "package.xml"]));

      if not Needs_Rebuild (Target, Sources) then
         return;
      end if;

      Run
        ("are",
         ["--lang=Ada",
          "-o",
          "generated_src_html",
          "--content-only",
          "--name-access",
          "--rule=" & Join_Path (["html", "package.xml"]),
          Join_Path (["html", "dist"])]);
      Prepend_To_File (Target, Pragma_1 & Pragma_2 & Pragma_3);
   end Build_Ada_Resources;

   procedure Build_Config_Codegen is
      Target        : constant String := Join_Path ([Working_Dir, "config_codegen", "bin", "config_codegen"]);
      Sources       : String_Vectors.Vector;
      Src_Files     : String_Vectors.Vector;
      VSS_Extra_Old : constant String := Getenv ("VSS_EXTRA_ALIRE_PREFIX").all;
      VSS_Text_Old  : constant String := Getenv ("VSS_TEXT_ALIRE_PREFIX").all;
      XMLAda_Old    : constant String := Getenv ("XMLADA_ALIRE_PREFIX").all;
      LibGPR_Old    : constant String := Getenv ("LIBGPR_ALIRE_PREFIX").all;
      GNATCOLL_Old  : constant String := Getenv ("GNATCOLL_ALIRE_PREFIX").all;
   begin
      Collect_Files (Join_Path ([Working_Dir, "config_codegen", "src"]), Src_Files, Recursive => True);
      for F of Src_Files loop
         if F'Length >= 4 and then F (F'Last - 3 .. F'Last) in ".adb" | ".ads" then
            Sources.Append (F);
         end if;
      end loop;
      Sources.Append (Join_Path ([Working_Dir, "config_codegen", "alire.toml"]));
      Sources.Append (Join_Path ([Working_Dir, "config_codegen", "config_codegen.gpr"]));

      if not Needs_Rebuild (Target, Sources) then
         return;
      end if;

      if Exists (Target) then
         --  Alire will sometimes do a full rebuild without updating the timestamp.
         Delete_File (Target);
      end if;

      Setenv ("VSS_EXTRA_ALIRE_PREFIX", "");
      Setenv ("VSS_TEXT_ALIRE_PREFIX", "");
      Setenv ("XMLADA_ALIRE_PREFIX", "");
      Setenv ("LIBGPR_ALIRE_PREFIX", "");
      Setenv ("GNATCOLL_ALIRE_PREFIX", "");
      Run ("alr", ["--chdir=config_codegen", "build", "--development"]);
      Setenv ("VSS_EXTRA_ALIRE_PREFIX", VSS_Extra_Old);
      Setenv ("VSS_TEXT_ALIRE_PREFIX", VSS_Text_Old);
      Setenv ("XMLADA_ALIRE_PREFIX", XMLAda_Old);
      Setenv ("LIBGPR_ALIRE_PREFIX", LibGPR_Old);
      Setenv ("GNATCOLL_ALIRE_PREFIX", GNATCOLL_Old);
   end Build_Config_Codegen;

   procedure Run_Config_Codegen is
      Stamp   : constant String := Join_Path ([Working_Dir, ".config_codegen_stamp"]);
      Sources : String_Vectors.Vector;
   begin
      Sources.Append (Join_Path ([Working_Dir, "prunt.gpr"]));
      Sources.Append (Join_Path ([Working_Dir, "alire.toml"]));
      Sources.Append (Join_Path ([Working_Dir, "config_codegen", "bin", "config_codegen"]));
      Collect_Files_Containing (Join_Path ([Working_Dir, "src"]), "Prunt_Config", Sources, Recursive => True);

      if not Needs_Rebuild (Stamp, Sources) then
         return;
      end if;

      if Exists (Stamp) then
         Delete_File (Stamp);
      end if;

      Run (Join_Path (["config_codegen", "bin", "config_codegen"]), ["-P", "prunt.gpr"]);

      declare
         F : File_Type;
      begin
         Create (F, Out_File, Stamp);
         Close (F);
      end;
   end Run_Config_Codegen;

begin
   if Ada.Environment_Variables.Value ("PRUNT_SKIP_PRE_BUILD", "false") = "true" then
      Put_Line ("Skipping prebuild due to PRUNT_SKIP_PRE_BUILD=true");
      return;
   end if;

   Ensure_Submodules;
   Build_Html_Dist;
   Clean_Dist_Stale;
   Build_Ada_Resources;
   Build_Config_Codegen;
   Run_Config_Codegen;
end Prebuild;
