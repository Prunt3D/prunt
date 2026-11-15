#!/usr/bin/env python3
# Part of the Prunt Motion Controller. Copyright (C) 2026 Liam Powell (liam@prunt3d.com).
#
# dimension_check is derived from GNAT's dimensionality checking, gcc/ada/sem_dim.adb, Copyright (C) 2011-2025, Free
# Software Foundation, Inc. It is free software under the GNU General Public License, version 3 or (at your option)
# any later version, WITHOUT ANY WARRANTY; see the file COPYING of this crate.
"""Rewrite Ada sources from GNAT's dimension aspects to the annotations that dimension_check reads.

    Dimension_System => ((Unit_Name => Millimeter, Unit_Symbol => "mm", Dim_Symbol => "Length"), ...)
becomes
    Annotate => (Dimension_Check, Dimension_System, "Millimeter", ...)

    Dimension => (Symbol => "mm/s", Millimeter => 1, Second => -1, others => 0)
becomes
    Annotate => (Dimension_Check, Dimension, "Millimeter", 1, "Second", -1)

and a rational exponent written as an integer division, which GNAT reads as a fraction for a dimensioned operand,
becomes a real exponent: ** (1 / 2) becomes ** 0.5 and ** (4 / 3) becomes ** (4.0 / 3.0). The real exponents need a
"**" (Left, Right : T'Base) return T'Base function, for example a renaming of the "**" of an instance of
Ada.Numerics.Generic_Elementary_Functions; this script does not add one.

Every rewrite keeps the number of lines, so line numbers stay valid. Exponents are rewritten by text, whatever the type
of their operand: check the listed rewrites, since an integer division exponent of a type outside a dimension system
means something else. Only named Dimension aggregates are understood; anything else is reported and left alone.

Usage: migrate_dimensions.py [--check] FILE...

Files are rewritten in place. With --check, nothing is written and the exit status is 1 if any file would change.
"""

import argparse
import re
import sys
from pathlib import Path


class MigrationError(Exception):
    pass


def code_mask(text):
    """Return a list of booleans: True for characters of code, False inside comments, strings and characters."""
    mask = [True] * len(text)
    i = 0
    n = len(text)
    while i < n:
        c = text[i]
        if c == "-" and text.startswith("--", i):
            j = text.find("\n", i)
            j = n if j < 0 else j
            for k in range(i, j):
                mask[k] = False
            i = j
        elif c == '"':
            j = i + 1
            while j < n:
                if text[j] == '"':
                    if j + 1 < n and text[j + 1] == '"':
                        j += 2
                        continue
                    break
                if text[j] == "\n":
                    break
                j += 1
            for k in range(i, min(j + 1, n)):
                mask[k] = False
            i = j + 1
        elif c == "'" and i + 2 < n and text[i + 2] == "'" and not (i > 0 and (text[i - 1].isalnum() or text[i - 1] in "_)")):
            for k in range(i, i + 3):
                mask[k] = False
            i += 3
        else:
            i += 1
    return mask


def matching_paren(text, mask, open_index):
    depth = 0
    for i in range(open_index, len(text)):
        if not mask[i]:
            continue
        if text[i] == "(":
            depth += 1
        elif text[i] == ")":
            depth -= 1
            if depth == 0:
                return i
    raise MigrationError("unbalanced parentheses")


def split_top_level(text):
    """Split the inside of an aggregate at commas that are not nested in parentheses or strings."""
    mask = code_mask(text)
    items = []
    depth = 0
    start = 0
    for i, c in enumerate(text):
        if not mask[i]:
            continue
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
        elif c == "," and depth == 0:
            items.append(text[start:i])
            start = i + 1
    items.append(text[start:])
    return [item.strip() for item in items if item.strip()]


def named(item):
    match = re.fullmatch(r"(\w+)\s*=>\s*(.*)", item, re.S)
    if not match:
        return None, item
    return match.group(1), match.group(2).strip()


def system_names(aggregate):
    names = []
    for item in split_top_level(aggregate):
        item = item.strip()
        if not (item.startswith("(") and item.endswith(")")):
            raise MigrationError("expected a parenthesized dimension in Dimension_System: " + item)
        fields = split_top_level(item[1:-1])
        unit = None
        for position, field in enumerate(fields):
            name, value = named(field)
            if name is None and position == 0:
                unit = value
            elif name is not None and name.lower() == "unit_name":
                unit = value
        if unit is None or not re.fullmatch(r"\w+", unit):
            raise MigrationError("no Unit_Name in " + item)
        names.append(unit)
    return names


def exponent_argument(value):
    text = re.sub(r"\s+", "", value)
    if re.fullmatch(r"[+-]?\d+", text):
        return str(int(text))
    match = re.fullmatch(r"\(?([+-]?)\(?(\d+)/(\d+)\)?\)?", text)
    if match:
        sign, numerator, denominator = match.groups()
        return '"%s%s/%s"' % ("-" if sign == "-" else "", numerator, denominator)
    raise MigrationError("cannot read the dimension exponent " + value)


def dimension_arguments(aggregate):
    arguments = []
    for item in split_top_level(aggregate):
        name, value = named(item)
        if name is None:
            raise MigrationError("positional Dimension aggregates are not supported: " + item)
        if name.lower() in ("symbol", "others"):
            continue
        exponent = exponent_argument(value)
        if exponent != "0":
            arguments.append('"%s", %s' % (name, exponent))
    return arguments


def with_lines(replacement, original):
    """Put the line breaks of original into replacement after its last commas, so that the line count is kept."""
    breaks = original.count("\n")
    if breaks == 0:
        return replacement
    indent = re.search(r"\n([ \t]*)\S", original)
    indent = indent.group(1) if indent else ""
    parts = replacement.split(", ")
    used = min(breaks, len(parts) - 1)
    lines = [", ".join(parts[: len(parts) - used])] + parts[len(parts) - used :]
    result = (",\n" + indent).join(lines)
    if breaks > used:
        result = result[:-1] + "\n" * (breaks - used) + indent + ")"
    return result


ASPECT = re.compile(r"\b(Dimension_System|Dimension)(\s*)=>(\s*)\(", re.I)
EXPONENT = re.compile(r"\*\*(\s*)\((\s*)(-?)(\s*)(\d+)(\s*)/(\s*)(\d+)(\s*)\)")


def migrate(text, filename, log):
    mask = code_mask(text)
    out = []
    position = 0
    for match in ASPECT.finditer(text):
        if match.start() < position or not mask[match.start()]:
            continue
        open_index = match.end() - 1
        close_index = matching_paren(text, mask, open_index)
        inner = text[open_index + 1 : close_index]
        if match.group(1).lower() == "dimension_system":
            replacement = "Annotate => (Dimension_Check, Dimension_System, %s)" % ", ".join(
                '"%s"' % name for name in system_names(inner)
            )
        else:
            replacement = "Annotate => (Dimension_Check, Dimension%s)" % "".join(
                ", " + argument for argument in dimension_arguments(inner)
            )
        original = text[match.start() : close_index + 1]
        out.append(text[position : match.start()])
        out.append(with_lines(replacement, original))
        position = close_index + 1
        line = text.count("\n", 0, match.start()) + 1
        log.append("%s:%d: rewrote %s aspect" % (filename, line, match.group(1)))
    out.append(text[position:])
    text = "".join(out)

    mask = code_mask(text)
    out = []
    position = 0
    for match in EXPONENT.finditer(text):
        if not mask[match.start()]:
            continue
        negative = match.group(3) == "-"
        numerator, denominator = int(match.group(5)), int(match.group(8))
        if denominator == 0:
            continue
        space = match.group(1)
        if numerator * 2 == denominator:
            replacement = "**" + space + ("(-0.5)" if negative else "0.5")
        else:
            replacement = "**%s(%s%d.0 / %d.0)" % (space, "-" if negative else "", numerator, denominator)
        out.append(text[position : match.start()])
        out.append(replacement)
        position = match.end()
        line = text.count("\n", 0, match.start()) + 1
        log.append("%s:%d: rewrote %s as %s" % (filename, line, match.group(0), replacement))
    out.append(text[position:])
    return "".join(out)


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--check", action="store_true", help="only report whether files would change")
    parser.add_argument("--quiet", action="store_true", help="do not list the rewrites")
    parser.add_argument("files", nargs="+", type=Path)
    args = parser.parse_args()

    changed = False
    failed = False
    for path in args.files:
        text = path.read_text(encoding="utf-8")
        log = []
        try:
            new_text = migrate(text, str(path), log)
        except MigrationError as error:
            print("%s: error: %s" % (path, error), file=sys.stderr)
            failed = True
            continue
        if new_text.count("\n") != text.count("\n"):
            print("%s: error: the rewrite would change the line count" % path, file=sys.stderr)
            failed = True
            continue
        if new_text != text:
            changed = True
            if not args.quiet:
                for entry in log:
                    print(entry)
            if not args.check:
                path.write_text(new_text, encoding="utf-8")
    if failed:
        return 2
    return 1 if args.check and changed else 0


if __name__ == "__main__":
    sys.exit(main())
