import re
import pprint
from dataclasses import dataclass
from typing import List, Optional, Dict, Any, Union
from collections import OrderedDict
import os

# I know this is messy, but it gets the job done. I wish you good luck if you ever need to modify this.

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

property_map_output_string = ""

def emit_for_pmap(text, level):
    global property_map_output_string
    property_map_output_string += "   " * level + str(text) + "\n"

reader_output_string = ""

def emit_for_reader(text, level):
    global reader_output_string
    reader_output_string += "   " * level + str(text) + "\n"

def handle_item(field: Union[Record, Enum, Array], outer: Field, path: str, level: int):
    if type(field) is Record and field.variant is not None and field.variant.discriminant == "Fixed_Kind":
        first = True
        for name in field.fields:
            emit_for_pmap(f'{"(" if first else "els"}if {outer.fixed_kind.replace("???", str(level - 1))} = {field.fields[name].key} then ', level)
            emit_for_reader(f'{"(" if first else "els"}if {outer.fixed_kind.replace("???", str(level - 1) + "\'Image")} = {field.fields[name].key} then (Fixed_Kind => {field.fields[name].key}, {name} => (', level)
            handle_item(data[field.fields[name].type], outer, path, level + 1)
            emit_for_reader("))", level)
            first = False
        emit_for_pmap("else raise Constraint_Error)", level)
        emit_for_reader("else raise Constraint_Error)", level)
    elif type(field) is Record:
        if field.variant is not None and field.variant.discriminant == "Kind":
            emit_for_pmap("Variant (", level)
        else:
            emit_for_pmap("Sequence (", level)
        emit_for_reader("(", level)
        emit_for_pmap(f'"{field.description}",', level)
        if field.variant is not None and field.variant.discriminant == "Kind":
            emit_for_pmap('"' + data[field.variant.type].values[field.variant.default].key + '",', level)
        first = True
        if len(field.fields) == 0:
            emit_for_pmap("[]", level)
            emit_for_reader("null record", level)
        for name in field.fields:
            if not first:
                emit_for_pmap("+", level)
            if field.fields[name].include_if:
                emit_for_pmap(f"(if {field.fields[name].include_if} then [", level)
            else:
                emit_for_pmap("[", level)
            if field.variant is not None and field.variant.discriminant == "Kind":
                emit_for_pmap(f'"{data[field.variant.type].values[field.fields[name].key].key}" =>', level)
                emit_for_reader(f"{"" if first else "els"}if Get (Data, \"{path}\") = \"{data[field.variant.type].values[field.fields[name].key].key}\" then (Kind => {data[field.variant.type].values[field.fields[name].key].name}, {data[field.variant.type].values[field.fields[name].key].name} =>", level)
            else:
                emit_for_pmap(f'"{field.fields[name].key}" =>', level)
                if not first:
                    emit_for_reader(",", level)
                emit_for_reader(f'{field.fields[name].name} =>', level)
                if field.fields[name].include_if:
                    emit_for_reader(f"(if {field.fields[name].include_if} then ", level)
            if field.fields[name].type in data:
                if field.variant is not None and field.variant.discriminant == "Kind":
                    handle_item(data[field.fields[name].type], field.fields[name], path + "$" + data[field.variant.type].values[field.fields[name].key].key, level + 1)
                else:
                    handle_item(data[field.fields[name].type], field.fields[name], path + "$" + field.fields[name].key, level + 1)
            elif field.fields[name].type == "Boolean":
                emit_for_pmap(f'Boolean ("{field.fields[name].description}", Default => {field.fields[name].default})', level)
                emit_for_reader(f'Boolean\'(Get (Data, "{path + "$" + field.fields[name].key}"))', level)
            elif field.fields[name].type in ("Velocity", "Acceleration", "Jerk", "Snap", "Crackle", "Time", "Length", "Dimensionless", "Current", "Voltage", "Inductance", "Resistance", "Temperature", "Frequency", "PWM_Scale"):
                emit_for_pmap(f'Float ("{field.fields[name].description}", Default => {field.fields[name].default}, Min => {field.fields[name].min}, Max => {field.fields[name].max}, Unit => "{field.fields[name].units or ""}")', level)
                emit_for_reader(f'Get (Data, "{path + "$" + field.fields[name].key}") {(" * " + field.fields[name].units) if field.fields[name].units is not None else ""}', level)
            elif field.fields[name].type.replace("'Base", "") in ("Heater_Name", "Fan_Name", "Laser_Name", "Thermistor_Name", "Input_Switch_Name", "Stepper_Name"):
                if field.fields[name].override_string_set:
                    emit_for_pmap(f'Discrete ("{field.fields[name].description}", Default => {field.fields[name].override_string_set}.First_Element, Options => {field.fields[name].override_string_set})', level)
                else:
                    emit_for_pmap(f'Discrete ("{field.fields[name].description}", Default => {field.fields[name].type.replace("'Base", "")}_Strings.First_Element, Options => {field.fields[name].type.replace("'Base", "")}_Strings)', level)
                emit_for_reader(f'{field.fields[name].type.replace("\'Base", "")}\'Value (Get (Data, "{path + "$" + field.fields[name].key}"))', level)
            elif field.fields[name].type == "Dimensionless_Ratio":
                assert field.fields[name].default == "(1.0, 1.0)"
                emit_for_pmap(f'Float_Ratio ("{field.fields[name].description}", Default_Numerator => 1.0, Default_Denominator => 1.0, Min => {field.fields[name].min}, Max => {field.fields[name].max})', level)
                emit_for_reader(f'(Get (Data, "{path + "$" + field.fields[name].key}$Numerator"), Get (Data, "{path + "$" + field.fields[name].key}$Denominator"))', level)
            elif field.fields[name].type.startswith("TMC_Types.Unsigned_") or field.fields[name].type == "Integer":
                emit_for_pmap(f'Integer ("{field.fields[name].description}", Default => {field.fields[name].default}, Min => {field.fields[name].min}, Max => {field.fields[name].max}, Unit => "{field.fields[name].units or ""}")', level)
                emit_for_reader(f'Get (Data, "{path + "$" + field.fields[name].key}")', level)
            elif field.fields[name].map is not None:
                emit_for_pmap(f'Discrete ("{field.fields[name].description}", Default => "{field.fields[name].map[field.fields[name].default]}", Options => [{", ".join(['"' + field.fields[name].map[x] + '"' for x in field.fields[name].map])}])', level)
                emit_for_reader(f'(', level)
                first_in_map = True
                for name_in_map in field.fields[name].map:
                    emit_for_reader(f'{"" if first_in_map else "els"}if UTF8_String\'(Get (Data, "{path + "$" + field.fields[name].key}")) = "{field.fields[name].map[name_in_map]}" then {name_in_map}', level)
                    first_in_map = False
                emit_for_reader(f'else raise Constraint_Error)', level)
            else:
                emit_for_pmap(field.fields[name], level)
                raise Exception("Type not implemented: " + str(field.fields[name].type))
            if field.variant is not None and field.variant.discriminant == "Kind":
                emit_for_reader(f')', level)
            if field.fields[name].include_if:
                emit_for_pmap("] else [])", level)
            else:
                emit_for_pmap("]", level)
            if field.fields[name].include_if and (field.variant is None or field.variant.discriminant != "Kind"):
                emit_for_reader(f"else {field.fields[name].default})", level)
            first = False
        if field.variant is not None and field.variant.discriminant == "Kind":
            emit_for_reader("else raise Constraint_Error)", level)
        else:
            emit_for_reader(")", level)
        emit_for_pmap(")", level)
    elif type(field) is Array:
        if field.element_type in data and type(data[field.element_type]) == Enum:
            emit_for_pmap(f'{"Tabbed_" if outer.tabbed else ""}Sequence ("{outer.description}", [for Index of {field.index_type}_Strings => Discrete ("", Default => "{data[field.element_type].values[outer.default].key}", Options => [{", ".join(['"' + data[field.element_type].values[x].key + '"' for x in data[field.element_type].values])}])])', level)
            emit_for_reader(f'[for Index_{str(level)} in {field.index_type} =>', level)
            emit_for_reader(f'(', level)
            first_in_map = True
            for value in data[field.element_type].values:
                emit_for_reader(f'{"" if first_in_map else "els"}if UTF8_String\'(Get (Data, "{path}$" & Index_{str(level)}\'Image)) = "{data[field.element_type].values[value].key}" then {value}', level)
                first_in_map = False
            emit_for_reader(f'else raise Constraint_Error)]', level)
        elif field.element_type in ("Velocity", "Acceleration", "Jerk", "Snap", "Crackle", "Time", "Length", "Dimensionless", "Current", "Voltage", "Inductance", "Resistance", "Temperature", "Frequency", "PWM_Scale"):
            emit_for_pmap(f'{"Tabbed_" if outer.tabbed else ""}Sequence ("{outer.description}", [for Index of {field.index_type}_Strings => Float ("", Default => {outer.default}, Min => {outer.min}, Max => {outer.max}, Unit => "{outer.units or ""}")])', level)
            emit_for_reader(f'[for Index_{str(level)} in {field.index_type} => Get (Data, "{path}$" & Index_{str(level)}\'Image) {(" * " + outer.units) if outer.units is not None else ""}]', level)
        elif type(data[field.element_type]) == Record:
            emit_for_pmap(f'{"Tabbed_" if outer.tabbed else ""}Sequence ("{outer.description}", [for Index_{str(level)} of {field.index_type}_Strings =>', level)
            emit_for_reader(f'[for Index_{str(level)} in {field.index_type} =>', level)
            handle_item(data[field.element_type], outer, path + "$" + f'" & Index_{str(level)}\'Image & "', level + 1)
            emit_for_pmap("])", level)
            emit_for_reader("]", level)
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
        emit_for_pmap("+", 0)
        emit_for_reader(",", 0)
    first = False
    emit_for_reader(f'{name} =>', 0)
    if data["User_Config"].fields[name].include_if:
        emit_for_pmap(f"(if {data["User_Config"].fields[name].include_if} then [", 0)
        emit_for_reader(f"(if {data["User_Config"].fields[name].include_if} then", 0)
    else:
        emit_for_pmap("[", 0)
    emit_for_pmap(f'"{data["User_Config"].fields[name].key}" =>', 0)
    handle_item(data[data["User_Config"].fields[name].type], data["User_Config"].fields[name], data["User_Config"].fields[name].key, 1)
    if data["User_Config"].fields[name].include_if:
        emit_for_pmap("] else [])", 0)
        emit_for_reader(f" else {data["User_Config"].fields[name].default})", 0)
    else:
        emit_for_pmap("]", 0)

with open("src/prunt-config-build_schema.adb", "w") as f:
    f.write("--  This file is automatically generated based on the contents of prunt-config.ads.\n")
    f.write("--  Run config_codegen.py to regenerate this file.\n")
    f.write("pragma Style_Checks (Off);\n")
    f.write("\n")
    f.write("separate (Prunt.Config)\n")
    f.write("function Build_Schema return Property_Maps.Map is\n")
    f.write("   pragma Unsuppress (All_Checks);\n")
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
    f.write(re.sub(r"\]\s*\+\s*\[", ", ", property_map_output_string))
    f.write(";\n")
    f.write("end Build_Schema;\n")
    f.write("pragma Style_Checks (On);\n")

with open("src/prunt-config-json_to_user_config.adb", "w") as f:
    f.write("--  This file is automatically generated based on the contents of prunt-config.ads.\n")
    f.write("--  Run config_codegen.py to regenerate this file.\n")
    f.write("pragma Style_Checks (Off);\n")
    f.write("with GNATCOLL;\n")
    f.write("with GNATCOLL.JSON;\n")
    f.write("with Prunt.TMC_Types;\n")
    f.write("with Prunt.TMC_Types.TMC2240;\n")
    f.write("--  I don't know why we need all this here, but it's the only thing that seems to make GNAT happy.\n")
    f.write("\n")
    f.write("separate (Prunt.Config)\n")
    f.write("function JSON_To_User_Config (Data : JSON_Value) return User_Config is\n")
    f.write("   pragma Unsuppress (All_Checks);\n")
    f.write("   use GNATCOLL;\n");
    f.write("   use GNATCOLL.JSON;\n");
    f.write("   use Prunt;\n")
    f.write("   use Prunt.TMC_Types;\n")
    f.write("   use Prunt.TMC_Types.TMC2240;\n")
    f.write("   function My_Get_Long_Float (Val : JSON_Value) return Long_Float is\n")
    f.write("   begin\n")
    f.write("      if Kind (Val) = JSON_Float_Type then\n")
    f.write("         return Get_Long_Float (Val);\n")
    f.write("      elsif Kind (Val) = JSON_Int_Type then\n")
    f.write("         return Long_Float (Long_Long_Integer'(Get (Val)));\n")
    f.write("      else\n")
    f.write("         raise Constraint_Error with \"Not a number.\";\n")
    f.write("      end if;\n")
    f.write("   end My_Get_Long_Float;\n")
    f.write("\n")
    f.write("   function My_Get_Long_Float (Val : JSON_Value; Field : UTF8_String) return Long_Float is\n")
    f.write("   begin\n")
    f.write("      return My_Get_Long_Float (Get (Val, Field));\n")
    f.write("   end My_Get_Long_Float;\n")
    f.write("\n")
    f.write("   generic\n");
    f.write("      type T is range <>;\n");
    f.write("   function Get_JSON_Integer (Val : JSON_Value; Field : UTF8_String) return T;\n");
    f.write("\n")
    f.write("   function Get_JSON_Integer (Val : JSON_Value; Field : UTF8_String) return T is\n")
    f.write("   begin\n")
    f.write("      return T (Long_Long_Integer'(Get (Get (Val, Field))));\n")
    f.write("   end Get_JSON_Integer;\n")
    f.write("\n")
    f.write("   pragma Warnings (Off, \"function \"\"Get\"\" is not referenced\");\n")
    f.write("   pragma Warnings (Off, \"cannot instantiate \"\"Get_JSON_Integer\"\" before body seen\");\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_1);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_2);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_3);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_4);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_5);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_6);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_7);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_8);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_9);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_10);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_11);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_12);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_13);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_14);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_15);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_16);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_17);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_18);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_19);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_20);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_21);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_22);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_23);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_24);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_25);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_26);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_27);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_28);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_29);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_30);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_31);\n")
    f.write("   function Get is new Get_JSON_Integer (Prunt.TMC_Types.Unsigned_32);\n")
    f.write("   function Get is new Get_JSON_Integer (Integer);\n")
    f.write("   pragma Warnings (On, \"cannot instantiate \"\"Get_JSON_Integer\"\" before body seen\");\n")
    f.write("   pragma Warnings (On, \"function \"\"Get\"\" is not referenced\");\n")
    f.write("\n")
    f.write("   function Get (Val : JSON_Value; Field : UTF8_String) return Dimensionless is\n")
    f.write("   begin\n")
    f.write("      return Dimensionless (My_Get_Long_Float (Val, Field));\n")
    f.write("   end Get;\n")
    f.write("\n")
    f.write("begin\n")
    f.write("   return (\n")
    f.write(reader_output_string)
    f.write(");\n")
    f.write("end JSON_To_User_Config;\n")
    f.write("pragma Style_Checks (On);\n")
