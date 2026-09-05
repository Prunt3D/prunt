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

--  This package provides a schema-validated configuration system.
--
--  Two copies of the configuration are kept: One for the configuration stored on disk and one for the live
--  configuration used by modules. The live configuration is accessed via Config_Data objects which give access to a
--  partial view of the configuration file and are intended to be passed to module instances when they are created. The
--  stored configuration is accessed via a single Config_File object, this is intended to be used for things such as
--  the web UI configuration editor. A procedure is provided to reset the state of the live configuration to that of
--  the stored configuration when Prunt is restarted.
--
--  The configuration can be edited in two ways. Firstly, modules may apply patches to the live configuration via
--  Config_Data objects, which are also applied to the stored configuration when a save procedure is called.
--  Secondly, the web UI or other parts of Prunt may edit the stored configuration via Config_File, these changes are
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
--                 ... as defined in the Config_Property_Parameters children below.
--              }
--           },
--           ...
--        }
--     }

pragma Extensions_Allowed (On);

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Tags;
with Prunt.Generic_Lock;
with Prunt.Indefinite_Ordered_Maps_With_Insertion_Order;
with Prunt.JSON;

private with Ada.Finalization;
private with Prunt.Limited_Shared_Pointers;
private with VSS.String_Vectors;

package Prunt.Config is

   package Discrete_String_Sets is new Ada.Containers.Ordered_Sets (Virtual_String);

   package Config_Data_Paths is new Ada.Containers.Vectors (Positive, Virtual_String);

   type Required_Config_Selection is record
      Path     : Config_Data_Paths.Vector;
      Selected : Virtual_String;
   end record;

   package Required_Config_Selection_Vectors is new Ada.Containers.Vectors (Positive, Required_Config_Selection);

   type Config_Path is record
      Path                : Config_Data_Paths.Vector;
      Required_Selections : Required_Config_Selection_Vectors.Vector;
   end record;

   type Config_Presentation_Condition is record
      Controller_Tag  : Ada.Tags.Tag;
      Controller_Path : Config_Path;
      Values          : Discrete_String_Sets.Set;
   end record;
   --  Display the property carrying this condition only when the value at Controller_Path is one of Values.
   --  Controller_Tag identifies the module supplying the property, independently of its configured owner name.
   --  Controller_Path is obtained from a typed, module-local path generated from that module's user configuration.

   No_Presentation_Condition : constant Config_Presentation_Condition :=
     (Controller_Tag => Ada.Tags.No_Tag, Controller_Path => <>, Values => []);

   type Config_Override is record
      Owner : Virtual_String;
      Path  : Config_Data_Paths.Vector;
      Value : JSON.JSON_Value;
   end record;
   --  Immutable replacement for one module-local configuration value.
   --
   --  Owner names the module, Path uses the same module-local path format as Config_Data.Get and Config_Data.Set,
   --  and Value is the JSON value to place at that path in the live/effective configuration.

   package Config_Override_Vectors is new Ada.Containers.Vectors (Positive, Config_Override);

   type Config_Error is record
      Path    : Config_Data_Paths.Vector;
      Message : Virtual_String;
   end record;

   package Config_Error_Vectors is new Ada.Containers.Vectors (Positive, Config_Error);

   type Config_Property_Parameters is tagged record
      Description  : Virtual_String;
      Experimental : Boolean := False;
      --  UI visibility only; hiding experimental options does not change stored values.
      Present_When : Config_Presentation_Condition := No_Presentation_Condition;
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

   type Config_Property_Parameters_String is new Config_Property_Parameters with record
      --  Schema format:
      --     {
      --        "Kind": "String",
      --        "Description": string,
      --        "Default": string
      --     }
      --
      --  Config format:
      --     string

      Default : Virtual_String;
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

   type Config_Data is private;
   --  All instances of Config_Data belonging to a Config_File must be finalised before the corresponding
   --  Config_File is finalised or else an error will be raised.
   --
   --  A copy or different instance of a Config_Data for the same module shares all values updated using Set with
   --  the original, including those set after the copy is made. Config_Data is just a wrapper around a reference to
   --  the Config_File except for those that come from Migrate, which should never be copied.
   --
   --  TODO: Enforce the above by making Config_Data a controlled type or giving it a controlled member.

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Boolean;
   --  Retrieve the Boolean configuration value at Path.

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Long_Float;
   --  Retrieve the Long_Float configuration value at Path.

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Dimensionless;
   --  Retrieve the dimensionless configuration value at Path.

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Long_Long_Integer;
   --  Retrieve the integer configuration value at Path.

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Virtual_String;
   --  Retrieve the string configuration value at Path.

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Dimensionless_Ratio;
   --  Retrieves a value from the configuration.
   --
   --  Data is shared between all Config_Data instances for the same module.
   --
   --  Raises Constraint_Error if:
   --  - The Path does not exist in the configuration data.
   --  - The value at Path is not compatible with the return type (e.g., trying to read a string as an integer).
   --  - The Path structure is invalid for the types traversed (e.g., requesting a field of a scalar).

   procedure Set (Data : in out Config_Data; Path : Config_Data_Paths.Vector; Value : Boolean);
   --  Replace the Boolean configuration value at Path.

   procedure Set (Data : in out Config_Data; Path : Config_Data_Paths.Vector; Value : Long_Float);
   --  Replace the Long_Float configuration value at Path.

   procedure Set (Data : in out Config_Data; Path : Config_Data_Paths.Vector; Value : Dimensionless);
   --  Replace the dimensionless configuration value at Path.

   procedure Set (Data : in out Config_Data; Path : Config_Data_Paths.Vector; Value : Long_Long_Integer);
   --  Replace the integer configuration value at Path.

   procedure Set (Data : in out Config_Data; Path : Config_Data_Paths.Vector; Value : Virtual_String);
   --  Replace the string configuration value at Path.

   procedure Set (Data : in out Config_Data; Path : Config_Data_Paths.Vector; Value : Dimensionless_Ratio);
   --  Updates a value in the configuration.
   --
   --  Changes are only in-memory until Save is called and no validation is performed until that point. Data is
   --  shared between all Config_Data instances for the same module.

   procedure Save (Data : in out Config_Data);
   --  Persists the current state of Data to the underlying Config_File.
   --
   --  This writes the entire configuration to disk, but only with updates from the relevant module, not all modules.
   --  Creates a backup of the previous file if it existed (appended with _backup_N).
   --
   --  Raises Constraint_Error if the data does not match the schema.

   function Module_Name (Data : Config_Data) return Virtual_String;
   --  Returns the name of the module which this object is for.

   function Resolve_Config_Path (Data : Config_Data; Path : Config_Path) return Config_Data_Paths.Vector;
   --  Return Path's raw representation after checking that all enclosing variant alternatives are selected and that
   --  the target exists in Data. Raises Constraint_Error when Path is not currently reportable.

   type Config_Schema_Version is new Positive;

   type Versioned_Config_Schema is tagged record
      Version             : Config_Schema_Version;
      Module_Instance_Tag : Ada.Tags.Tag := Ada.Tags.No_Tag;
      Top_Level_Items     : Config_Property_Maps.Map;
   end record;

   procedure Migrate (This : Versioned_Config_Schema; Old_Version : Config_Schema_Version; Data : in out Config_Data)
   is null;
   --  When this procedure is called any new fields in the current schema version will be available in Data as well
   --  as the old fields. Any fields not present in the new schema will be removed after this procedure returns.
   --
   --  Data must not be copied.
   --
   --  TODO: Enforce the above by making Config_Data a controlled type or giving it a controlled member.

   package Config_Schema_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Virtual_String, Versioned_Config_Schema'Class);

   type Config_File (<>) is limited private;

   function Create
     (File_Name : String; Schemas : Config_Schema_Maps.Map; Overrides : Config_Override_Vectors.Vector := [])
      return Config_File;
   --  Initializes access to a configuration file.
   --
   --  Behaviour:
   --  - For each module in Schemas:
   --    - If present in file:
   --      - Checks "Version".
   --      - If File.Version < Schema.Version, Migrate is called.
   --      - If File.Version > Schema.Version, raises Constraint_Error.
   --      - If Versions match, structure is validated against Schema.
   --    - If missing in file, it is initialized with defaults.
   --  - If the file contains modules NOT in Schemas, Constraint_Error is raised.
   --  - If the file does not exist, it is created with default values for all modules in Schemas.
   --  - Overrides are module-local paths and JSON values which are validated against Schemas at startup.
   --  - Overrides are applied to the live configuration only. Overridden fields are removed from the stored
   --    configuration and generated schema, and attempts to modify them are rejected.

   function Get_Data (This : Config_File; Module_Name : Virtual_String) return Config_Data;
   --  Retrieves the configuration data for a specific module.
   --
   --  The returned Config_Data object provides access to the live configuration for the specified module. Changes
   --  made via this object are local to the live configuration until Save is called.

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
   --  The Value string must be a JSON object following the top-level structure:
   --    {
   --       "Prunt config version": 1,
   --       "Config": {
   --          "ModuleName": { "Version": N, "Config": { ... } }
   --       }
   --    }
   --
   --  It performs a recursive merge and calls Report for any validation errors encountered (e.g. unknown modules,
   --  extra fields, type mismatches). If any part of the validation fails then no part of the patch is applied.
   --
   --  Value is always set to the contents of the stored config.

   procedure Reset_Live_To_Stored (This : Config_File);
   --  Discard all pending changes in the live configuration and revert to the version stored on disk.

   type Save_Counter is range 0 .. 2 ** 63 - 1;

   function Last_Save (This : Config_File) return Save_Counter;
   --  Incremented when Save is called of a Config_Data.

private

   use Prunt.JSON;

   package File_Access_Lock is new Generic_Lock;
   --  Anything that touches a file uses this as multiple Config_File objects may refer to the same file.
   --
   --  TODO: We should only allow a single writer to exist for any given file.

   package JSON_Delta_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Virtual_String, JSON_Value);

   function Create_Default_Property_Config (Property : Config_Property_Parameters'Class) return JSON_Value;
   --  Build the configuration JSON value described by Property's defaults, recursively for composite properties.

   procedure Validate_Field_Names
     (Val            : JSON_Value;
      Allowed_Fields : VSS.String_Vectors.Virtual_String_Vector;
      Report         : access procedure (Path : Config_Data_Paths.Vector; Message : Virtual_String);
      Path           : Config_Data_Paths.Vector);
   --  Validates that all fields in Val are present in Allowed_Fields. Calls Report for any unknown fields found.

   procedure Set_Field (Val : JSON_Value; Field : Virtual_String; Value : Dimensionless);
   --  Store Value in Field using the JSON floating-point representation used for dimensionless values.

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
   --  Report values in Config that do not conform to Schema, optionally including missing fields.

   function Create_Default_Module_Config (Schema : Config_Property_Maps.Map) return JSON_Value;
   --  Build a module configuration object populated with every default in Schema.

   function Generate_Schemas_String (Schemas : Config_Schema_Maps.Map) return Virtual_String;
   --  Serialize Schemas in the JSON format consumed by the configuration user interface.

   procedure Recursive_Left_Merge (Left : JSON_Value; Right : JSON_Value; Full_Join : Boolean := True);
   --  Takes the content of the Right JSON object and places it into Left. Objects are recursively merged rather
   --  than being overwritten. All other types are overwritten with the values from Right.
   --
   --  If Full_Join is False then no keys which only exist in Right will be added to Left.

   protected type Config_File_Internal is
      --  The file IO in here is all potentially blocking, but the global lock means it should never cause an issue as
      --  long as nothing external accesses the config files.

      procedure Initialize
        (File_Name_In : String;
         Schemas_In   : Config_Schema_Maps.Map;
         Overrides_In : Config_Override_Vectors.Vector;
         Migrate      :
           access function
             (Module : Virtual_String; Old_Version : Config_Schema_Version; Old_Config : JSON_Value) return JSON_Value;
         Lock         : File_Access_Lock.Lock_Holder := File_Access_Lock.Lock);
      --  Reads the configuration file, validates it, performs migrations if necessary, and prepares the separate
      --  stored/UI and live/effective configuration views.

      function Get (Owner : Virtual_String; Path : Config_Data_Paths.Vector) return JSON_Value;
      --  Gets a value from the live configuration. Owner specifies the module requesting the data.

      procedure Set (Owner : Virtual_String; Path : Config_Data_Paths.Vector; Value : JSON_Value);
      --  Sets a value in the live configuration. Owner specifies the module setting the data.
      --
      --  Raises Constraint_Error if Path overlaps an override for Owner.

      procedure Save (Owner : Virtual_String; Lock : File_Access_Lock.Lock_Holder := File_Access_Lock.Lock);
      --  Saves the current live configuration to the stored configuration and to disk for the relevant module only.
      --  Owner specifies the module triggering the save.

      procedure Apply_Untrusted_Patch
        (Value  : Virtual_String;
         Result : out Virtual_String;
         Errors : out Config_Error_Vectors.Vector;
         Lock   : File_Access_Lock.Lock_Holder := File_Access_Lock.Lock);
      --  Applies a JSON patch to the configuration. Checks for validation errors before applying and does not apply
      --  any changes if validation fails. Patches which touch overridden fields are rejected.

      function Get_Stored_Config return Virtual_String;
      --  Returns the JSON string of the stored configuration.

      function Get_Schemas return Virtual_String;
      --  Returns the JSON string of the schemas.

      function Last_Save return Save_Counter;
      --  Returns the a counter value which is incremented every time the stored configuration is changed.

      procedure Reset_Live_To_Stored (Check_Ref_Count : access procedure);
      --  Resets the live configuration to match the stored configuration. Check_Ref_Count is called immediately upon
      --  entry and may be used to raise an exception if there are still references to the configuration file which
      --  are not expecting the live configuration to change. Overrides are reapplied after the reset.
   private
      procedure Write_File;
      File_Name       : Virtual_String := "";
      Live_Config     : JSON_Value := JSON_Null;
      Update_Deltas   : JSON_Delta_Maps.Map := [];
      Stored_Config   : JSON_Value := JSON_Null;
      Schemas         : Config_Schema_Maps.Map := [];
      Cached_Schemas  : Virtual_String := "";
      Save_Count      : Save_Counter := 0;
      Overrides       : Config_Override_Vectors.Vector := [];
      Visible_Schemas : Config_Schema_Maps.Map := [];
   end Config_File_Internal;

   package Config_File_Internal_Shared_Pointers is new Limited_Shared_Pointers (Config_File_Internal);

   type Config_File is new Ada.Finalization.Limited_Controlled with record
      Internal : Config_File_Internal_Shared_Pointers.Ref := Config_File_Internal_Shared_Pointers.Null_Ref;
   end record;

   overriding
   procedure Finalize (Object : in out Config_File);
   --  During finalisation we check that all Config_Data instances are finalised as the modules that hold them should
   --  be finalised before the relevant Config_File is.
   --
   --  Excluding this check would not lead to any memory safety issues. This check is just to make sure that modules
   --  are not misbehaving.

   function Get_JSON_Node
     (Root : JSON_Value; Path : Config_Data_Paths.Vector; Module : Virtual_String) return JSON_Value;
   --  Return the node at Path, raising Constraint_Error with Module context when a path component is absent.

   procedure Set_JSON_Node (Root : JSON_Value; Path : Config_Data_Paths.Vector; Value : JSON_Value);
   --  Store Value at Path, creating missing intermediate JSON objects.

   function Is_Path_Prefix (Prefix : Config_Data_Paths.Vector; Path : Config_Data_Paths.Vector) return Boolean;
   --  Returns True when Prefix is an initial segment of Path. The empty path is a prefix of every path.

   function Paths_Overlap (Left : Config_Data_Paths.Vector; Right : Config_Data_Paths.Vector) return Boolean;
   --  Returns True when either path is the other path or is inside the other path.

   function Path_Equals_Override
     (Owner : Virtual_String; Path : Config_Data_Paths.Vector; Overrides : Config_Override_Vectors.Vector)
      return Boolean;
   --  Returns True when Overrides contains an entry for Owner with exactly Path.

   function Path_Overlaps_Overrides
     (Owner : Virtual_String; Path : Config_Data_Paths.Vector; Overrides : Config_Override_Vectors.Vector)
      return Boolean;
   --  Returns True when Path matches, is inside, or contains an overridden path for Owner.

   function Unset_JSON_Node (Root : JSON_Value; Path : Config_Data_Paths.Vector) return Boolean;
   --  Removes the object member addressed by Path from Root. Returns True only when a member was removed.

   procedure Apply_Overrides_To_Config
     (Config : JSON_Value; Schemas : Config_Schema_Maps.Map; Overrides : Config_Override_Vectors.Vector);
   --  Applies Overrides to the live/effective configuration object. Override values are cloned into Config.

   function Prune_Overrides_From_Module_Config
     (Owner         : Virtual_String;
      Module_Config : JSON_Value;
      Module_Schema : Config_Property_Maps.Map;
      Overrides     : Config_Override_Vectors.Vector) return Boolean;
   --  Removes all overridden fields for Owner from Module_Config. Returns True when Module_Config changed.

   function Prune_Overrides_From_Config
     (Config : JSON_Value; Schemas : Config_Schema_Maps.Map; Overrides : Config_Override_Vectors.Vector)
      return Boolean;
   --  Removes all overridden fields from the stored/UI configuration. Returns True when Config changed.

   function Prune_Overrides_From_Schemas
     (Schemas : Config_Schema_Maps.Map; Overrides : Config_Override_Vectors.Vector) return Config_Schema_Maps.Map;
   --  Returns a schema map suitable for the web UI, with overridden fields removed.

   procedure Validate_Overrides (Schemas : Config_Schema_Maps.Map; Overrides : Config_Override_Vectors.Vector);
   --  Validates override owners, paths, overlap, value types, and schema constraints.
   --
   --  Raises Constraint_Error if any override is invalid.

   procedure Validate_No_Overrides_In_Patch
     (Owner     : Virtual_String;
      Value     : JSON_Value;
      Overrides : Config_Override_Vectors.Vector;
      Report    : access procedure (Path : Config_Data_Paths.Vector; Message : Virtual_String));
   --  Reports every overridden path touched by an untrusted patch for Owner.

   function Path_Without_Last (Path : Config_Data_Paths.Vector) return Config_Data_Paths.Vector;
   --  Return Path without its final component, or an empty path when Path has fewer than two components.

   function Selected_Variant_Default
     (Schema : Config_Property_Maps.Map; Path : Config_Data_Paths.Vector; Default_Value : out JSON_Value)
      return Boolean;
   --  Return True and the enclosing variant's default object when Path selects that variant's Selected field.

   function Prune_Path_For_Override
     (Schema : Config_Property_Maps.Map; Path : Config_Data_Paths.Vector) return Config_Data_Paths.Vector;
   --  Return the schema path whose complete value must be hidden when Path is overridden.

   function Try_Get_JSON_Node
     (Root : JSON_Value; Path : Config_Data_Paths.Vector; Result : out JSON_Value) return Boolean;
   --  Look up Path without raising, returning False and JSON_Null when it cannot be traversed.

   procedure Merge_Default_JSON_Node (Root : JSON_Value; Path : Config_Data_Paths.Vector; Default_Value : JSON_Value);
   --  Ensure Path contains Default_Value's structure while preserving any compatible existing object members.

   type Config_Data is record
      For_Migration    : Boolean := False;
      Module           : Virtual_String := "";
      Internal         : Config_File_Internal_Shared_Pointers.Ref := Config_File_Internal_Shared_Pointers.Null_Ref;
      --  Setting this to null by default means we will get an error if a Config_Data is default-initialized and
      --  then used. we need to allow default initialization as we store these in protected objects.
      Migration_Config : JSON_Value := JSON_Null;
   end record;

end Prunt.Config;
