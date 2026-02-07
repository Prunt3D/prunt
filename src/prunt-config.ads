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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Prunt.Generic_Lock;
with Prunt.Indefinite_Ordered_Maps_With_Insertion_Order;

private with Ada.Finalization;
private with Prunt.JSON;
private with Prunt.Limited_Shared_Pointers;
private with VSS.String_Vectors;

--  This package provides a schema-validated configuration system.
--
--  Two copies of the configuration are kept: One for the configuration stored on disk and one for the live
--  configuration used by modules. The live configuration is accessed via `Config_Data` objects which give access to a
--  partial view of the configuration file and are intended to be passed to module instances when they are created. The
--  stored configuration is accessed via a single `Config_File` object, this is intended to be used for things such as
--  the web UI configuration editor. A procedure is provided to reset the state of the live configuration to that of
--  the stored configuration when Prunt is restarted.
--
--  The configuration can be edited in two ways. Firstly, modules may apply patches to the live configuration via
--  `Config_Data` objects, which are also applied to the stored configuration when a save procedure is called.
--  Secondly, the web UI or other parts of Prunt may edit the stored configuration via `Config_File`, these changes are
--  not mirrored to the live configuration until the reset procedure is called.
--
--  The configuration file and schema are JSON object with the following structure:
--
--     {
--        "Prunt config version": 1,
--        "Config": {
--           "My_Module": {
--              "Version": 1,
--              "Config": {
--                 ... as defined in the `Config_Property_Parameters` children below.
--              }
--           },
--           ...
--        }
--     }

package Prunt.Config is

   package Discrete_String_Sets is new Ada.Containers.Ordered_Sets (Virtual_String);

   package Config_Data_Paths is new Ada.Containers.Vectors (Positive, Virtual_String);

   type Config_Error is record
      Path    : Config_Data_Paths.Vector;
      Message : Virtual_String;
   end record;

   package Config_Error_Vectors is new Ada.Containers.Vectors (Positive, Config_Error);

   type Config_Property_Parameters is tagged record
      Description : Virtual_String;
   end record;

   package Config_Property_Maps is new
     Indefinite_Ordered_Maps_With_Insertion_Order (Virtual_String, Config_Property_Parameters'Class);

   type Config_Property_Parameters_Boolean is new Config_Property_Parameters with record
      --  Schema format:
      --     {
      --        "Kind": "Boolean",
      --        "Description": string,
      --        "Default": boolean
      --     }
      --
      --  Config format:
      --     boolean

      Default : Boolean;
   end record;

   type Config_Property_Parameters_Discrete is new Config_Property_Parameters with record
      --  Schema format:
      --     {
      --        "Kind": "Discrete",
      --        "Description": string,
      --        "Options": [string, ...],
      --        "Default": string
      --     }
      --
      --  Config format:
      --     string

      Default : Virtual_String;
      Options : Discrete_String_Sets.Set;
   end record;

   type Config_Property_Parameters_Integer is new Config_Property_Parameters with record
      --  Schema format:
      --     {
      --        "Kind": "Integer",
      --        "Description": string,
      --        "Min": number,
      --        "Max": number,
      --        "Unit": string,
      --        "Default": number
      --     }
      --
      --  Config format:
      --     number

      Min     : Long_Long_Integer;
      Max     : Long_Long_Integer;
      Unit    : Virtual_String;
      Default : Long_Long_Integer;
   end record;

   type Config_Property_Parameters_Float is new Config_Property_Parameters with record
      --  Schema format:
      --     {
      --        "Kind": "Float",
      --        "Description": string,
      --        "Min": number,
      --        "Max": number,
      --        "Unit": string,
      --        "Default": number
      --     }
      --
      --  Config format:
      --     number

      Min     : Dimensionless;
      Max     : Dimensionless;
      Unit    : Virtual_String;
      Default : Dimensionless;
   end record;

   type Config_Property_Parameters_Float_Ratio is new Config_Property_Parameters with record
      --  Schema format:
      --     {
      --        "Kind": "Float_Ratio",
      --        "Description": string,
      --        "Min": number,
      --        "Max": number,
      --        "Default_Numerator": number,
      --        "Default_Denominator": number
      --     }
      --
      --  Config format:
      --     {
      --        "Numerator": number,
      --        "Denominator": number
      --     }

      Min     : Dimensionless;
      Max     : Dimensionless;
      Default : Dimensionless_Ratio;
   end record;

   type Config_Property_Parameters_Sequence is new Config_Property_Parameters with record
      --  Schema format:
      --     {
      --        "Kind": "Sequence",
      --        "Tabbed": boolean,
      --        "Description": string,
      --        "Children": {string: Config_Property_Parameters'Class (schema format), ...}
      --     }
      --
      --  Config format:
      --     {string: Config_Property_Parameters'Class (config format), ...}

      Tabbed   : Boolean;
      Children : Config_Property_Maps.Map;
   end record;

   type Config_Property_Parameters_Variant is new Config_Property_Parameters with record
      --  Schema format:
      --     {
      --        "Kind": "Variant",
      --        "Description": string,
      --        "Default": string,
      --        "Children": {string: Config_Property_Parameters'Class (schema format), ...}
      --     }
      --
      --  Config format:
      --     {"Selected": string, "Children": {string: Config_Property_Parameters'Class (config format), ...}}
      --
      --  All children are always present, regardless of the selected value.

      Default  : Virtual_String;
      Children : Config_Property_Maps.Map;
   end record;

   type Config_Data (<>) is private;
   --  All instances of `Config_Data` belonging to a `Config_File` must be finalised before the corresponding
   --  `Config_File` is finalised or else an error will be raised.

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Boolean;
   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Long_Float;
   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Dimensionless;
   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Long_Long_Integer;
   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Virtual_String;
   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Dimensionless_Ratio;
   --  Retrieves a value from the configuration.
   --
   --  Data is shared between all `Config_Data` instances for the same module.
   --
   --  Raises `Constraint_Error` if:
   --  - The Path does not exist in the configuration data.
   --  - The value at Path is not compatible with the return type (e.g., trying to read a string as an integer).
   --  - The Path structure is invalid for the types traversed (e.g., requesting a field of a scalar).

   procedure Set (Data : Config_Data; Path : Config_Data_Paths.Vector; Value : Boolean);
   procedure Set (Data : Config_Data; Path : Config_Data_Paths.Vector; Value : Long_Float);
   procedure Set (Data : Config_Data; Path : Config_Data_Paths.Vector; Value : Dimensionless);
   procedure Set (Data : Config_Data; Path : Config_Data_Paths.Vector; Value : Long_Long_Integer);
   procedure Set (Data : Config_Data; Path : Config_Data_Paths.Vector; Value : Virtual_String);
   procedure Set (Data : Config_Data; Path : Config_Data_Paths.Vector; Value : Dimensionless_Ratio);
   --  Updates a value in the configuration.
   --
   --  Changes are only in-memory until `Save` is called and no validation is performed until that point. Data is
   --  shared between all `Config_Data` instances for the same module.

   procedure Save (Data : Config_Data);
   --  Persists the current state of `Data` to the underlying `Config_File`.
   --
   --  This writes the entire configuration to disk, but only with updates from the relevant module, not all modules.
   --  Creates a backup of the previous file if it existed (appended with _backup_N).
   --
   --  Raises `Constraint_Error` if the data does not match the schema.

   type Config_Schema_Version is new Positive;

   type Versioned_Config_Schema is tagged record
      Version         : Config_Schema_Version;
      Top_Level_Items : Config_Property_Maps.Map;
   end record;

   procedure Migrate (This : Versioned_Config_Schema; Old_Version : Config_Schema_Version; Data : in out Config_Data)
   is null;
   --  When this procedure is called any new fields in the current schema version will be available in `Data` as well
   --  as the old fields. Any fields not present in the new schema will be removed after this procedure returns.

   package Config_Schema_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Virtual_String, Versioned_Config_Schema'Class);

   type Config_File (<>) is limited private;

   function Create (File_Name : String; Schemas : Config_Schema_Maps.Map) return Config_File;
   --  Initializes access to a configuration file.
   --
   --  Behaviour:
   --  - For each module in `Schemas`:
   --    - If present in file:
   --      - Checks "Version".
   --      - If File.Version < Schema.Version, `Migrate` is called.
   --      - If File.Version > Schema.Version, raises `Constraint_Error`.
   --      - If Versions match, structure is validated against Schema.
   --    - If missing in file, it is initialized with defaults.
   --  - If the file contains modules NOT in `Schemas`, `Constraint_Error` is raised.
   --  - If the file does not exist, it is created with default values for all modules in `Schemas`.

   function Get_Data (This : Config_File; Module_Name : Virtual_String) return Config_Data;
   --  Retrieves the configuration data for a specific module.
   --
   --  The returned `Config_Data` object provides access to the live configuration for the specified module. Changes
   --  made via this object are local to the live configuration until `Save` is called.

   function Get_Schema_String (This : Config_File) return Virtual_String;
   --  Returns the JSON string representation of the configuration schema.

   function Get_Data_String (This : Config_File) return Virtual_String;
   --  Returns the JSON string representation of the stored configuration.

   procedure Apply_Untrusted_Patch
     (This   : Config_File;
      Value  : Virtual_String;
      Result : out Virtual_String;
      Errors : out Config_Error_Vectors.Vector);
   --  Applies a JSON patch to the configuration.
   --
   --  The `Value` string must be a JSON object following the top-level structure:
   --    {
   --       "Prunt config version": 1,
   --       "Config": {
   --          "ModuleName": { "Version": N, "Config": { ... } }
   --       }
   --    }
   --
   --  It performs a recursive merge and calls `Report` for any validation errors encountered (e.g. unknown modules,
   --  extra fields, type mismatches). If any part of the validation fails then no part of the patch is applied.
   --
   --  `Value` is always set to the contents of the stored config.

   procedure Reset_Live_To_Stored (This : Config_File);
   --  Discard all pending changes in the live configuration and revert to the version stored on disk.

   type Save_Counter is range 0 .. 2 ** 63 - 1;

   function Last_Save (This : Config_File) return Save_Counter;
   --  Incremented when `Save` is called of a `Config_Data`.

private

   use Prunt.JSON;

   package File_Access_Lock is new Generic_Lock;
   --  Anything that touches a file uses this as multiple `Config_File` objects may refer to the same file.
   --
   --  TODO: We should only allow a single writer to exist for any given file.

   package JSON_Delta_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Virtual_String, JSON_Value);

   procedure Validate_Field_Names
     (Val            : JSON_Value;
      Allowed_Fields : VSS.String_Vectors.Virtual_String_Vector;
      Report         : access procedure (Path : Config_Data_Paths.Vector; Message : Virtual_String);
      Path           : Config_Data_Paths.Vector);
   --  Validates that all fields in `Val` are present in `Allowed_Fields`. Calls `Report` for any unknown fields found.

   procedure Set_Field (Val : JSON_Value; Field : Virtual_String; Value : Dimensionless);

   procedure Validate_Outer_Config_Structure
     (Config : JSON_Value; Report : access procedure (Path : Config_Data_Paths.Vector; Message : Virtual_String));
   --  Validates the top-level structure of the configuration JSON. Checks for the presence of "Prunt config version"
   --  and "Config" fields.

   procedure Validate_Module_Config_Structure
     (Config : JSON_Value; Report : access procedure (Path : Config_Data_Paths.Vector; Message : Virtual_String));
   --  Validates the structure of a module's configuration. Checks for "Version" and "Config" fields.

   procedure Validate_Module_Config_To_Schema
     (Config                   : JSON_Value;
      Schema                   : Config_Property_Maps.Map;
      Report                   : access procedure (Path : Config_Data_Paths.Vector; Message : Virtual_String);
      Check_For_Missing_Fields : Boolean);

   function Create_Default_Module_Config (Schema : Config_Property_Maps.Map) return JSON_Value;

   function Generate_Schemas_String (Schemas : Config_Schema_Maps.Map) return Virtual_String;

   procedure Recursive_Left_Merge (Left : JSON_Value; Right : JSON_Value; Full_Join : Boolean := True);
   --  Takes the content of the `Right` JSON object and places it into `Left`. Objects are recursively merged rather
   --  than being overwritten. All other types are overwritten with the values from `Right`.
   --
   --  If `Full_Join` is False then no keys which only exist in `Right` will be added to `Left`.

   protected type Config_File_Internal is
      --  The file IO in here is all potentially blocking, but the global lock means it should never cause an issue as
      --  long as nothing external accesses the config files.
      procedure Initialize
        (File_Name_In : String;
         Schemas_In   : Config_Schema_Maps.Map;
         Migrate      :
           access function
             (Module : Virtual_String; Old_Version : Config_Schema_Version; Old_Config : JSON_Value) return JSON_Value;
         Lock         : File_Access_Lock.Lock_Holder := File_Access_Lock.Lock);
      --  Reads the configuration file, validates it, and performs migrations if necessary.
      function Get (Owner : Virtual_String; Path : Config_Data_Paths.Vector) return JSON_Value;
      --  Gets a value from the live configuration. `Owner` specifies the module requesting the data.
      procedure Set (Owner : Virtual_String; Path : Config_Data_Paths.Vector; Value : JSON_Value);
      --  Sets a value in the live configuration. `Owner` specifies the module setting the data.
      procedure Save (Owner : Virtual_String; Lock : File_Access_Lock.Lock_Holder := File_Access_Lock.Lock);
      --  Saves the current live configuration to the stored configuration and to disk for the relevant module only.
      --  `Owner` specifies the module triggering the save.
      procedure Apply_Untrusted_Patch
        (Value  : Virtual_String;
         Result : out Virtual_String;
         Errors : out Config_Error_Vectors.Vector;
         Lock   : File_Access_Lock.Lock_Holder := File_Access_Lock.Lock);
      --  Applies a JSON patch to the configuration. Checks for validation errors before applying and does not apply
      --  any changes if validation fails.
      function Get_Stored_Config return Virtual_String;
      --  Returns the JSON string of the stored configuration.
      function Get_Schemas return Virtual_String;
      --  Returns the JSON string of the schemas.
      function Last_Save return Save_Counter;
      --  Returns the a counter value which is incremented every time the stored configuration is changed.
      procedure Reset_Live_To_Stored (Check_Ref_Count : access procedure);
      --  Resets the live configuration to match the stored configuration. `Check_Ref_Count` is called immdiately upon
      --  entry and may be used to raise an exception if there are still references to the configuration file which
      --  are not expecting the live configuration to change.
   private
      procedure Write_File;
      File_Name      : Virtual_String := "";
      Live_Config    : JSON_Value := JSON_Null;
      Update_Deltas  : JSON_Delta_Maps.Map := [];
      Stored_Config  : JSON_Value := JSON_Null;
      Schemas        : Config_Schema_Maps.Map := [];
      Cached_Schemas : Virtual_String := "";
      Save_Count     : Save_Counter := 0;
   end Config_File_Internal;

   package Config_File_Internal_Shared_Pointers is new Limited_Shared_Pointers (Config_File_Internal);

   type Config_File is new Ada.Finalization.Limited_Controlled with record
      Internal : Config_File_Internal_Shared_Pointers.Ref := Config_File_Internal_Shared_Pointers.Null_Ref;
   end record;

   overriding
   procedure Finalize (Object : in out Config_File);
   --  During finalisation we check that all `Config_Data` instances are finalised as the modules that hold them should
   --  be finalised before the relevant `Config_File` is.
   --
   --  Excluding this check would not lead to any memory safety issues. This check is just to make sure that modules
   --  are not misbehaving.

   function Get_JSON_Node
     (Root : JSON_Value; Path : Config_Data_Paths.Vector; Module : Virtual_String) return JSON_Value;
   procedure Set_JSON_Node (Root : JSON_Value; Path : Config_Data_Paths.Vector; Value : JSON_Value);

   type Config_Data is record
      For_Migration    : Boolean;
      Module           : Virtual_String;
      Internal         : Config_File_Internal_Shared_Pointers.Ref;
      Migration_Config : JSON_Value := JSON_Null;
   end record;

end Prunt.Config;
