import re
import pprint
from dataclasses import dataclass
from typing import List, Optional, Dict, Any, Union
from collections import OrderedDict
import os

# I know this is messy, but it gets the job done.

@dataclass
class EnumValue:
    name: str
    key: Optional[str] = None

@dataclass
class Field:
    name: str
    type: str
    default: Optional[str] = None
    key: Optional[str] = None
    description: Optional[str] = None
    min: Optional[str] = None
    max: Optional[str] = None
    map: Optional[Dict[str, str]] = None
    fixed_kind: Optional[str] = None
    kind: Optional[str] = None
    units: Optional[str] = None
    include_if: Optional[str] = None
    override_string_set: Optional[str] = None
    tabbed: bool = False

@dataclass
class Variant:
    discriminant: str
    type: str
    default: Optional[str] = None

@dataclass
class Record:
    name: str
    fields: OrderedDict[str, Field]
    description: Optional[str] = None
    variant: Optional[Variant] = None

@dataclass
class Enum:
    name: str
    values: Dict[str, EnumValue]

@dataclass
class Array:
    name: str
    index_type: str
    element_type: str


def parse_config_ads(content: str) -> Dict[str, Union[Record, Enum, Array]]:
    in_config_block = False
    config_lines = []
    for line in content.splitlines():
        if "--!PRUNT END USER CONFIG DECLARATIONS" in line:
            in_config_block = False
            break
        if in_config_block:
            config_lines.append(line)
        if "--!PRUNT BEGIN USER CONFIG DECLARATIONS" in line:
            in_config_block = True

    extracted_data: Dict[str, Union[Record, Enum, Array]] = {}
    i = 0
    while i < len(config_lines):
        line = config_lines[i].strip()

        record_match = re.match(r"type\s+(\w+)(?:\s*\(([^)]+)\))?\s+is\s+record", line)
        array_match = re.match(r"type\s+(\w+)\s+is\s+array\s*\(([^)]+)\)\s+of\s+([^;]+);", line)

        if record_match:
            record_name = record_match.group(1)
            variant_part = record_match.group(2)

            variant = None
            if variant_part:
                variant_match = re.match(r"(\w+)\s*:\s*([\w\._]+)(?:\s*:=\s*(\w+))?", variant_part.strip())
                if variant_match:
                    variant = Variant(
                        discriminant=variant_match.group(1),
                        type=variant_match.group(2),
                        default=variant_match.group(3)
                    )

            i += 1

            description = None
            if i < len(config_lines) and config_lines[i].strip().startswith("--  Description:"):
                desc = config_lines[i].strip().replace("--  Description:", "").strip()
                i += 1
                while i < len(config_lines) and config_lines[i].strip().startswith("--") and not re.search(r'--\s+\w+:', config_lines[i]):
                    desc += " " + config_lines[i].strip().replace("--", "").strip()
                    i += 1
                description = desc

            fields = OrderedDict()
            while i < len(config_lines) and not re.match(r"end\s+record;", config_lines[i].strip()):
                field_line = config_lines[i].strip()
                field_match = re.match(r"(\w+)\s*:\s*([^;]+?)(?:\s*:=\s*(.+))?;", field_line)

                if field_match:
                    field_name = field_match.group(1)
                    field_type = field_match.group(2).strip()
                    field_default = field_match.group(3)
                    field_units = None
                    if field_default:
                        field_default = field_default.strip()
                        if field_default.startswith("("):
                            match = re.match(r"\(others => ([\d\._E\-]+)\s*\*\s*([\w\s/\*\*]+)\)", field_default)
                            if match:
                                field_default = match.group(1).strip()
                                field_units = match.group(2).strip()
                            else:
                                match = re.match(r"\(others => ([^<]+)\)", field_default)
                                if match:
                                    field_default = match.group(1).strip()
                                    field_units = None
                        else:
                            match = re.match(r"([\d\._E\-]+)\s*\*\s*([\w\s/\*\*]+)", field_default)
                            if match:
                                field_default = match.group(1).strip()
                                field_units = match.group(2).strip()

                    i += 1
                    comment_lines = []
                    while i < len(config_lines) and config_lines[i].strip().startswith("--"):
                        comment_lines.append(config_lines[i].strip())
                        i += 1

                    field = Field(name=field_name, type=field_type, default=field_default, units=field_units)
                    k = 0
                    while k < len(comment_lines):
                        comment_line = comment_lines[k]
                        if "--  Key:" in comment_line:
                            field.key = comment_line.replace("--  Key:", "").strip()
                            k += 1
                        elif "--  Description:" in comment_line:
                            desc = comment_line.replace("--  Description:", "").strip()
                            k += 1
                            while k < len(comment_lines) and re.search(r'--    ', comment_lines[k]):
                                desc += " " + comment_lines[k].replace("--", "").strip()
                                k += 1
                            field.description = desc
                        elif "--  Min:" in comment_line:
                            field.min = comment_line.replace("--  Min:", "").strip()
                            k += 1
                        elif "--  Max:" in comment_line:
                            field.max = comment_line.replace("--  Max:", "").strip()
                            k += 1
                        elif "--  Map:" in comment_line:
                            map_str = comment_line.replace("--  Map:", "").strip()
                            k += 1
                            while k < len(comment_lines) and "=>" in comment_lines[k]:
                                map_str += " " + comment_lines[k].replace("--", "").strip()
                                k += 1

                            map_data = {}
                            pairs = re.findall(r'"([^"]+)"\s*=>\s*([\w\._]+)', map_str)
                            for value, key in pairs:
                                map_data[key] = value
                            field.map = map_data
                        elif "--  Fixed_Kind:" in comment_line:
                            field.fixed_kind = comment_line.replace("--  Fixed_Kind:", "").strip()
                            k += 1
                        elif "--  Kind:" in comment_line:
                            field.kind = comment_line.replace("--  Kind:", "").strip()
                            k += 1
                        elif "--  Include_If:" in comment_line:
                            field.include_if = comment_line.replace("--  Include_If:", "").strip()
                            k += 1
                        elif "--  Override_String_Set:" in comment_line:
                            field.override_string_set = comment_line.replace("--  Override_String_Set:", "").strip()
                            k += 1
                        elif "--  Tabbed: True" in comment_line:
                            field.tabbed = True
                            k += 1
                        else:
                            k += 1

                    fields[field_name] = field
                else:
                    i += 1

            extracted_data[record_name] = Record(name=record_name, fields=fields, description=description, variant=variant)
            i += 1
        elif array_match:
            array_name = array_match.group(1)
            index_type = array_match.group(2)
            element_type = array_match.group(3).strip()
            i += 1
            extracted_data[array_name] = Array(name=array_name, index_type=index_type, element_type=element_type)
        else:
            enum_match = re.match(r"type\s+([\w_]+)\s+is\s*(.*)", line)
            if enum_match:
                enum_name = enum_match.group(1)
                rest_of_line = enum_match.group(2).strip()

                if not rest_of_line:
                    i += 1
                    rest_of_line = config_lines[i].strip()

                if rest_of_line.startswith("("):
                    lines_for_enum = []
                    rest_of_line = rest_of_line[1:]

                    if ');' in rest_of_line:
                        lines_for_enum.append(rest_of_line.split(');')[0])
                    else:
                        lines_for_enum.append(rest_of_line)
                        i += 1
                        while i < len(config_lines):
                            line_content = config_lines[i]
                            if ');' in line_content:
                                lines_for_enum.append(line_content.split(');')[0])
                                break
                            lines_for_enum.append(line_content)
                            i += 1

                    values: Dict[str, EnumValue] = OrderedDict()
                    last_val = ""
                    for l in lines_for_enum:
                        l = l.strip()
                        if not l:
                            continue

                        if l.startswith('--'):
                            key_match = re.search(r"Key:\s*(.*)", l)
                            if key_match and values:
                                values[last_val].key = key_match.group(1).strip()
                        else:
                            value_names = [v.strip() for v in l.split(',') if v.strip()]
                            for v_name in value_names:
                                if v_name:
                                    values[v_name] = EnumValue(name=v_name)
                                    last_val = v_name

                    extracted_data[enum_name] = Enum(name=enum_name, values=values)
                    i += 1
                else:
                    i += 1
            else:
                i += 1

    return extracted_data

output_string = ""

def emit(text):
    global output_string
    output_string += str(text) + "\n"

def emit_property_map(field: Union[Record, Enum, Array], outer: Field):
    if type(field) is Record and field.variant is not None and field.variant.discriminant == "Fixed_Kind":
        first = True
        for name in field.fields:
            emit(f'{"(" if first else "els"}if {outer.fixed_kind} = {field.fields[name].key} then ')
            emit_property_map(data[field.fields[name].type], outer)
            first = False
        emit("else raise Constraint_Error)")
    elif type(field) is Record:
        if field.variant is not None and field.variant.discriminant == "Kind":
            emit("Variant (")
        else:
            emit("Sequence (")
        emit(f'"{field.description}",')
        if field.variant is not None and field.variant.discriminant == "Kind":
            emit('"' + data[field.variant.type].values[field.variant.default].key + '",')
        first = True
        if len(field.fields) == 0:
            emit("[]")
        for name in field.fields:
            if not first:
                emit("+")
            first = False
            if field.fields[name].include_if:
                emit(f"(if {field.fields[name].include_if} then [")
            else:
                emit("[")
            if field.variant is not None and field.variant.discriminant == "Kind":
                emit(f'"{data[field.variant.type].values[field.fields[name].key].key}" =>')
            else:
                emit(f'"{field.fields[name].key}" =>')

            if field.fields[name].type in data:
                emit_property_map(data[field.fields[name].type], field.fields[name])
            elif field.fields[name].type == "Boolean":
                emit(f'Boolean ("{field.fields[name].description}", Default => {field.fields[name].default})')
            elif field.fields[name].type in ("Velocity", "Acceleration", "Jerk", "Snap", "Crackle", "Time", "Length", "Dimensionless", "Current", "Voltage", "Inductance", "Resistance", "Temperature", "Frequency", "PWM_Scale"):
                emit(f'Float ("{field.fields[name].description}", Default => {field.fields[name].default}, Min => {field.fields[name].min}, Max => {field.fields[name].max}, Unit => "{field.fields[name].units or ""}")')
            elif field.fields[name].type in ("Heater_Name", "Fan_Name", "Laser_Name", "Thermistor_Name", "Input_Switch_Name", "Stepper_Name"):
                if field.fields[name].override_string_set:
                    emit(f'Discrete ("{field.fields[name].description}", Default => {field.fields[name].override_string_set}.First_Element, Options => {field.fields[name].override_string_set})')
                else:
                    emit(f'Discrete ("{field.fields[name].description}", Default => {field.fields[name].type}_Strings.First_Element, Options => {field.fields[name].type}_Strings)')
            elif field.fields[name].type == "Dimensionless_Ratio":
                assert field.fields[name].default == "(1.0, 1.0)"
                emit(f'Float_Ratio ("{field.fields[name].description}", Default_Numerator => 1.0, Default_Denominator => 1.0, Min => {field.fields[name].min}, Max => {field.fields[name].max})')
            elif field.fields[name].type.startswith("TMC_Types.Unsigned_") or field.fields[name].type == "Integer":
                emit(f'Integer ("{field.fields[name].description}", Default => {field.fields[name].default}, Min => {field.fields[name].min}, Max => {field.fields[name].max}, Unit => "{field.fields[name].units or ""}")')
            elif field.fields[name].map is not None:
                emit(f'Discrete ("{field.fields[name].description}", Default => "{field.fields[name].map[field.fields[name].default]}", Options => [{", ".join(['"' + field.fields[name].map[x] + '"' for x in field.fields[name].map])}])')
            else:
                emit(field.fields[name])
                raise Exception("Type not implemented: " + str(field.fields[name].type))

            if field.fields[name].include_if:
                emit("] else [])")
            else:
                emit("]")
        emit(")")
    elif type(field) is Array:
        if field.element_type in data and type(data[field.element_type]) == Enum:
            emit(f'{"Tabbed_" if outer.tabbed else ""}Sequence ("{outer.description}", [for Index of {field.index_type}_Strings => Discrete ("", Default => "{data[field.element_type].values[outer.default].key}", Options => [{", ".join(['"' + data[field.element_type].values[x].key + '"' for x in data[field.element_type].values])}])])')
        elif field.element_type in ("Velocity", "Acceleration", "Jerk", "Snap", "Crackle", "Time", "Length", "Dimensionless", "Current", "Voltage", "Inductance", "Resistance", "Temperature", "Frequency", "PWM_Scale"):
            emit(f'{"Tabbed_" if outer.tabbed else ""}Sequence ("{outer.description}", [for Index of {field.index_type}_Strings => Float ("", Default => {outer.default}, Min => {outer.min}, Max => {outer.max}, Unit => "{outer.units or ""}")])')
        elif type(data[field.element_type]) == Record:
            emit(f'{"Tabbed_" if outer.tabbed else ""}Sequence ("{outer.description}", [for Index of {field.index_type}_Strings =>')
            emit_property_map(data[field.element_type], outer)
            emit("])")
        else:
            raise Exception("Type not implemented: " + str(field.element_type))
    else:
        raise Exception("Type not implemented: " + type(field))

with open("src/prunt-config.ads", "r") as f:
    content = f.read()

data = parse_config_ads(content)
data["Position"] = Array(name="Position", index_type="Axis_Name", element_type="Length")
data["Position_Scale"] = Array(name="Position", index_type="Axis_Name", element_type="Dimensionless")

# pp = pprint.PrettyPrinter(width=120)
# pp.pprint(data)

assert type(data["User_Config"]) is Record

first = True
for name in data["User_Config"].fields:
    if not first:
        emit("+")
    first = False
    if data["User_Config"].fields[name].include_if:
        emit(f"(if {data["User_Config"].fields[name].include_if} then [")
    else:
        emit("[")
    emit(f'"{data["User_Config"].fields[name].key}" =>')
    emit_property_map(data[data["User_Config"].fields[name].type], data["User_Config"].fields[name])
    if data["User_Config"].fields[name].include_if:
        emit("] else [])")
    else:
        emit("]")

with open("src/prunt-config-build_schema.adb", "w") as f:
    f.write("--  This file is automatically generated based on the contents of prunt-config.ads.\n")
    f.write("--  Run config_codegen.py to regenerate this file.\n")
    f.write("pragma Style_Checks (Off);\n")
    f.write("\n")
    f.write("separate (Prunt.Config)\n")
    f.write("function Build_Schema return Property_Maps.Map is\n")
    f.write("   use type Property_Maps.Map;\n")
    f.write("\n")
    f.write("   function Boolean (Description : String; Default : Boolean) return Property_Parameters_Access is\n")
    f.write("   begin\n")
    f.write("      return\n")
    f.write("        new Property_Parameters'\n")
    f.write("          (Kind => Boolean_Kind, Description => To_Unbounded_String (Description), Boolean_Default => Default);\n")
    f.write("   end Boolean;\n")
    f.write("\n")
    f.write("   function Sequence (Description : String; Children : Property_Maps.Map) return Property_Parameters_Access is\n")
    f.write("   begin\n")
    f.write("      return\n")
    f.write("        new Property_Parameters'\n")
    f.write("          (Kind              => Sequence_Kind,\n")
    f.write("           Description       => To_Unbounded_String (Description),\n")
    f.write("           Sequence_Children => Children,\n")
    f.write("           Sequence_Tabbed   => False);\n")
    f.write("   end Sequence;\n")
    f.write("\n")
    f.write("   function Tabbed_Sequence (Description : String; Children : Property_Maps.Map) return Property_Parameters_Access is\n")
    f.write("   begin\n")
    f.write("      return\n")
    f.write("        new Property_Parameters'\n")
    f.write("          (Kind              => Sequence_Kind,\n")
    f.write("           Description       => To_Unbounded_String (Description),\n")
    f.write("           Sequence_Children => Children,\n")
    f.write("           Sequence_Tabbed   => True);\n")
    f.write("   end Tabbed_Sequence;\n")
    f.write("\n")
    f.write("   function Variant\n")
    f.write("     (Description : String; Default : String; Children : Property_Maps.Map) return Property_Parameters_Access is\n")
    f.write("   begin\n")
    f.write("      return\n")
    f.write("        new Property_Parameters'\n")
    f.write("          (Kind             => Variant_Kind,\n")
    f.write("           Description      => To_Unbounded_String (Description),\n")
    f.write("           Variant_Children => Children,\n")
    f.write("           Variant_Default  => To_Unbounded_String (Default));\n")
    f.write("   end Variant;\n")
    f.write("\n")
    f.write("   function Integer\n")
    f.write("     (Description : String; Default, Min, Max : Long_Long_Integer; Unit : String) return Property_Parameters_Access is\n")
    f.write("   begin\n")
    f.write("      return\n")
    f.write("        new Property_Parameters'\n")
    f.write("          (Kind            => Integer_Kind,\n")
    f.write("           Description     => To_Unbounded_String (Description),\n")
    f.write("           Integer_Min     => Min,\n")
    f.write("           Integer_Max     => Max,\n")
    f.write("           Integer_Unit    => To_Unbounded_String (Unit),\n")
    f.write("           Integer_Default => Default);\n")
    f.write("   end Integer;\n")
    f.write("\n")
    f.write("   function Float\n")
    f.write("     (Description : String; Default, Min, Max : Long_Float; Unit : String) return Property_Parameters_Access is\n")
    f.write("   begin\n")
    f.write("      return\n")
    f.write("        new Property_Parameters'\n")
    f.write("          (Kind          => Float_Kind,\n")
    f.write("           Description   => To_Unbounded_String (Description),\n")
    f.write("           Float_Min     => Min,\n")
    f.write("           Float_Max     => Max,\n")
    f.write("           Float_Unit    => To_Unbounded_String (Unit),\n")
    f.write("           Float_Default => Default);\n")
    f.write("   end Float;\n")
    f.write("\n")
    f.write("   function Float_Ratio\n")
    f.write("     (Description : String; Default_Numerator, Default_Denominator, Min, Max : Long_Float)\n")
    f.write("      return Property_Parameters_Access is\n")
    f.write("   begin\n")
    f.write("      return\n")
    f.write("        new Property_Parameters'\n")
    f.write("          (Kind                            => Float_Ratio_Kind,\n")
    f.write("           Description                     => To_Unbounded_String (Description),\n")
    f.write("           Float_Ratio_Min                 => Min,\n")
    f.write("           Float_Ratio_Max                 => Max,\n")
    f.write("           Float_Ratio_Default_Numerator   => Default_Numerator,\n")
    f.write("           Float_Ratio_Default_Denominator => Default_Denominator);\n")
    f.write("   end Float_Ratio;\n")
    f.write("\n")
    f.write("   function Discrete\n")
    f.write("     (Description : String; Default : String; Options : Discrete_String_Sets.Set) return Property_Parameters_Access is\n")
    f.write("   begin\n")
    f.write("      return\n")
    f.write("        new Property_Parameters'\n")
    f.write("          (Kind             => Discrete_Kind,\n")
    f.write("           Description      => To_Unbounded_String (Description),\n")
    f.write("           Discrete_Options => Options,\n")
    f.write("           Discrete_Default => To_Unbounded_String (Default));\n")
    f.write("   end Discrete;\n")
    f.write("\n")
    f.write("   Input_Switch_Name_Strings        : constant Discrete_String_Sets.Set :=\n")
    f.write("     Discrete_String_Sets.Difference\n")
    f.write("       ([for I in Input_Switch_Name => (if Input_Switch_Visible_To_User (I) then I'Image else \"\")], [\"\"]);\n")
    f.write("   StallGuard2_Stepper_Name_Strings : constant Discrete_String_Sets.Set :=\n")
    f.write("     Discrete_String_Sets.Difference\n")
    f.write("       ([for S in Stepper_Name => (if Stepper_Hardware (S).Kind in TMC2240_UART_Kind then S'Image else \"\")], [\"\"]);\n")
    f.write("   StallGuard4_Stepper_Name_Strings : constant Discrete_String_Sets.Set :=\n")
    f.write("     Discrete_String_Sets.Difference\n")
    f.write("       ([for S in Stepper_Name => (if Stepper_Hardware (S).Kind in TMC2240_UART_Kind then S'Image else \"\")], [\"\"]);\n")
    f.write("   Thermistor_Name_Strings          : constant Discrete_String_Sets.Set := [for T in Thermistor_Name => T'Image];\n")
    f.write("   Heater_Name_Strings              : constant Discrete_String_Sets.Set := [for H in Heater_Name => H'Image];\n")
    f.write("   Fan_Name_Strings                 : constant Discrete_String_Sets.Set := [for F in Fan_Name => F'Image];\n")
    f.write("   Laser_Name_Strings               : constant Discrete_String_Sets.Set := [for L in Laser_Name => L'Image];\n")
    f.write("   Stepper_Name_Strings             : constant Discrete_String_Sets.Set := [for S in Stepper_Name => S'Image];\n")
    f.write("   Axis_Name_Strings                : constant Discrete_String_Sets.Set := [for A in Axis_Name => A'Image];\n")
    f.write("begin\n")
    f.write("   return\n")
    f.write(re.sub(r"\]\s*\+\s*\[", ", ", output_string))
    f.write(";\n")
    f.write("end Build_Schema;\n")
    f.write("pragma Style_Checks (On);\n")
