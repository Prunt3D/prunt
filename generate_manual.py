import os
import re

def parse_gcode_commands(src_dir):
    gcode_commands = {}

    for filename in os.listdir(src_dir):
        if not filename.endswith(".ads"):
            continue

        filepath = os.path.join(src_dir, filename)
        with open(filepath, 'r') as f:
            lines = f.readlines()

        content = "".join(lines)

        # Find annotations like: with Annotate => (Prunt_Config, Gcode_Command, "M106");
        # We need to find the procedure declaration before it, and the comment after it.
        # It's easier to find the annotation, then work backwards for the procedure, and forwards for the comment.

        matches = re.finditer(r'with\s+Annotate\s*=>\s*\(Prunt_Config,\s*Gcode_Command,\s*"([^"]+)"\);', content)
        for match in matches:
            gcode = match.group(1)
            annot_start = match.start()
            annot_end = match.end()

            # Backwards search for 'procedure <name>'
            before_annot = content[:annot_start]
            proc_match = re.search(r'procedure\s+(\w+)(.*?)$', before_annot, re.DOTALL | re.MULTILINE)
            # Find the LAST procedure declaration before the annotation
            proc_matches = list(re.finditer(r'procedure\s+(\w+)', before_annot))
            if not proc_matches:
                continue

            last_proc = proc_matches[-1]
            proc_name = last_proc.group(1)

            # Extract arguments between the procedure name and the annotation
            args_section = before_annot[last_proc.end():]

            # Look for '(' and ')' to extract parameters
            args_str = ""
            bracket_start = args_section.find('(')
            if bracket_start != -1:
                # Find matching closing bracket
                bracket_end = args_section.rfind(')')
                if bracket_end != -1:
                    args_str = args_section[bracket_start+1:bracket_end]

            args = []
            if args_str:
                for arg_part in args_str.split(';'):
                    arg_part = arg_part.strip()
                    if not arg_part: continue
                    # Remove trailing comments
                    arg_part = re.sub(r'--.*', '', arg_part).strip()
                    if ':' in arg_part:
                        arg_names, arg_type = arg_part.split(':', 1)
                        arg_names = [name.strip() for name in arg_names.split(',')]
                        arg_type = arg_type.strip()
                        arg_type = re.sub(r'\s*:=.*', '', arg_type) # Remove defaults
                        for arg_name in arg_names:
                            if arg_name not in ["This", "Self_Ref", "Planner", "Command_Identifier", "Args", "Config_Data"]:
                                args.append(f"{arg_name}: {arg_type}")

            # Forwards search for the description
            # The description is the block of `--` comments immediately following the annotation.
            after_annot = content[annot_end:]
            desc_lines = []
            for line in after_annot.split('\n'):
                line = line.strip()
                if not line:
                    continue
                if line.startswith('--'):
                    desc_lines.append(line.lstrip('-').strip())
                else:
                    break

            desc = " ".join(desc_lines).strip()

            if gcode not in gcode_commands:
                gcode_commands[gcode] = []

            gcode_commands[gcode].append({
                "proc": proc_name,
                "args": args,
                "desc": desc
            })

    return gcode_commands

def parse_config_options(src_dir):
    user_configs = {}

    for filename in os.listdir(src_dir):
        if not filename.endswith(".ads"):
            continue

        filepath = os.path.join(src_dir, filename)
        with open(filepath, 'r') as f:
            content = f.read()

        # Extract records ending in User_Config or Root_User_Config
        # Because nested types exist, we can use a simpler approach:
        # Find "type XXX is record ... end record"
        # and see if it has the annotation afterwards.
        # Or look for the annotation and work backwards.

        matches = re.finditer(r'type\s+(\w+).*?is\s+(?:record(.*?)end record|array\s*\((.*?)\)\s*of\s*(.*?))\s*(?:with.*?)?Annotate\s*=>\s*\(Prunt_Config,\s*(?:User_Config|Root_User_Config)', content, re.DOTALL)

        for match in matches:
            type_name = match.group(1)
            record_body = match.group(2)
            array_index = match.group(3)
            array_type = match.group(4)

            fields = []
            if record_body:
                # Naive field extraction
                # Exclude lines starting with -- or case/when
                lines = record_body.split('\n')
                for line in lines:
                    line = line.strip()
                    if not line or line.startswith('--') or line.startswith('case') or line.startswith('when') or line.startswith('end case'):
                        continue

                    if ':' in line:
                        # Extract inline comments
                        desc = ""
                        if '--' in line:
                            code_part, comment_part = line.split('--', 1)
                            desc = comment_part.strip()
                            line = code_part.strip()
                        else:
                            line = line.strip()

                        # Handle semicolon
                        if line.endswith(';'):
                            line = line[:-1].strip()

                        # Ignore lines that don't look like field declarations
                        if '=>' in line or '(' in line.split(':')[0]:
                            continue

                        try:
                            names_str, type_str = line.split(':', 1)
                            names = [n.strip() for n in names_str.split(',')]
                            type_str = type_str.strip()
                            type_str = re.sub(r'\s*:=.*', '', type_str) # Remove defaults

                            for name in names:
                                if name and name != "null":
                                    fields.append({"name": name, "type": type_str, "desc": desc})
                        except ValueError:
                            pass
            elif array_type:
                fields.append({"name": "Array", "type": f"array({array_index}) of {array_type}", "desc": "Array type"})

            user_configs[type_name] = fields

    return user_configs

def generate_manual(src_dir, output_file):
    gcode_commands = parse_gcode_commands(src_dir)
    user_configs = parse_config_options(src_dir)

    with open(output_file, 'w') as f:
        f.write("# Prunt User Manual\n\n")

        f.write("## 1. Introduction\n")
        f.write("Welcome to the Prunt Motion Controller User Manual.\n\n")

        f.write("## 2. Configuration Reference\n")
        f.write("The following sections describe the available configuration options.\n\n")

        for type_name, fields in sorted(user_configs.items()):
            if type_name == "Module":
                continue

            f.write(f"### `{type_name}`\n")
            if fields:
                f.write("| Field | Type | Description |\n")
                f.write("|---|---|---|\n")
                for field in fields:
                    f.write(f"| `{field['name']}` | `{field['type']}` | {field['desc']} |\n")
            else:
                f.write("*(Type definition not explicitly parsed or no fields)*\n")
            f.write("\n")

        f.write("## 3. G-code Command Reference\n")
        f.write("The following G-code commands are supported by Prunt:\n\n")

        def sort_key(gcode):
            match = re.match(r'([A-Z])(\d+)', gcode)
            if match:
                return (match.group(1), int(match.group(2)))
            return (gcode, 0)

        for gcode in sorted(gcode_commands.keys(), key=sort_key):
            f.write(f"### `{gcode}`\n")
            for cmd in gcode_commands[gcode]:
                if cmd["desc"]:
                    f.write(f"{cmd['desc']}\n\n")
                if cmd["args"]:
                    f.write("**Parameters:**\n")
                    for arg in cmd["args"]:
                        f.write(f"- `{arg}`\n")
            f.write("\n")

if __name__ == "__main__":
    generate_manual("src", "USER_MANUAL.md")
