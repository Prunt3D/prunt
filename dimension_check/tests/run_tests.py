#!/usr/bin/env python3
# Part of the Prunt Motion Controller. Copyright (C) 2026 Liam Powell (liam@prunt3d.com).
#
# dimension_check is derived from GNAT's dimensionality checking, gcc/ada/sem_dim.adb, Copyright (C) 2011-2025, Free
# Software Foundation, Inc. It is free software under the GNU General Public License, version 3 or (at your option)
# any later version, WITHOUT ANY WARRANTY; see the file COPYING of this crate.
"""Run the tests of dimension_check. Build it first with `alr build` in the crate directory.

1. Expectation tests: every source in tests/cases marks the findings expected on a line with comments:

       L := T;   --! error: dimensions mismatch in assignment

   "--! error:", "--! warning:" and "--! note:" are expected in both modes; "--! extra error:" and
   "--! extra warning:" only without --gnat-compatible. The text after the colon must occur in the message. A
   statement without an error or warning marker must have no error or warning; notes are only checked where marked.
   Findings and markers are compared by statement, not by line, so that formatting can spread a statement over
   several lines.

2. GNAT parity: the sources in tests/gnat use GNAT's Dimension_System and Dimension aspects and the rational
   exponents GNAT reads. GNAT compiles them, and tools/migrate_dimensions.py rewrites copies of them into
   tests/out/gnat for dimension_check. The statements where GNAT reports a dimension error or an "assumed to be"
   warning must be exactly those where dimension_check --gnat-compatible reports one, and a subset of those where
   dimension_check reports one with its extra checks. A statement marked "--! GNAT false positive" must have a GNAT
   finding and none from dimension_check.

Usage: run_tests.py [-v]
"""

import argparse
import collections
import re
import shutil
import subprocess
import sys
from pathlib import Path

TESTS = Path(__file__).resolve().parent
CRATE = TESTS.parent
TOOL = CRATE / "bin" / "dimension_check"
MIGRATE = CRATE / "tools" / "migrate_dimensions.py"

TERMINATORS = (";", " is", " then", " loop", "begin", " do", " else", "record", "private", "=>")


def code_part(line):
    """Return line without its comment, ignoring "--" inside string literals."""
    in_string = False
    for index, char in enumerate(line):
        if char == '"':
            in_string = not in_string
        elif not in_string and line.startswith("--", index):
            return line[:index]
    return line


def statement_lines(path):
    """Map each line number of path to the first line of the statement or declaration it belongs to.

    A statement ends on a line whose code ends with ";" or a keyword that opens a block, so a formatter may spread one
    over several lines. A line with only a comment belongs to the code line before it, so a marker that a formatter
    moves below "if ... then" still applies to that line.
    """
    result = {}
    start = None
    previous = 1
    for number, line in enumerate(path.read_text(encoding="utf-8").splitlines(), start=1):
        code = code_part(line).rstrip()
        if not code.strip():
            result[number] = result.get(previous, number)
            continue
        if start is None:
            start = number
        result[number] = start
        previous = number
        if code.endswith(TERMINATORS):
            start = None
    return result


FINDING = re.compile(r"^(?P<file>[^:]+):(?P<line>\d+):(?P<column>\d+): (?P<kind>error|warning|note): (?P<message>.*)$")
MARKER = re.compile(r"--!\s*(?P<extra>extra\s+)?(?P<kind>error|warning|note):\s*(?P<text>.*?)\s*(?=--!|$)")


def run_tool(project, extra_args, cwd, spans):
    result = subprocess.run(
        [str(TOOL), "-P", str(project), "-v", *extra_args], cwd=cwd, capture_output=True, text=True
    )
    findings = collections.defaultdict(list)
    for line in result.stdout.splitlines():
        match = FINDING.match(line)
        if match:
            name, number = Path(match["file"]).name, int(match["line"])
            key = (name, spans.get(name, {}).get(number, number))
            findings[key].append((match["kind"], match["message"]))
        elif line.strip():
            print("unexpected output: " + line)
    return findings


def spans_of(directory):
    return {source.name: statement_lines(source) for source in directory.glob("*.ad[sb]")}


def expectations(directory, gnat_compatible, spans):
    expected = collections.defaultdict(list)
    for source in sorted(directory.glob("*.ad[sb]")):
        for number, text in enumerate(source.read_text(encoding="utf-8").splitlines(), start=1):
            for marker in MARKER.finditer(text):
                if marker["extra"] and gnat_compatible:
                    continue
                expected[(source.name, spans[source.name][number])].append((marker["kind"], marker["text"]))
    return expected


def check_expectations(verbose):
    failures = 0
    for gnat_compatible in (False, True):
        mode = "--gnat-compatible" if gnat_compatible else "default"
        args = ["--gnat-compatible"] if gnat_compatible else []
        spans = spans_of(TESTS / "cases")
        actual = run_tool(TESTS / "cases" / "cases.gpr", args, CRATE, spans)
        expected = expectations(TESTS / "cases", gnat_compatible, spans)
        checked = 0
        for key in sorted(set(actual) | set(expected)):
            found = list(actual.get(key, []))
            for kind, text in expected.get(key, []):
                checked += 1
                for index, (found_kind, message) in enumerate(found):
                    if found_kind == kind and text in message:
                        del found[index]
                        break
                else:
                    failures += 1
                    print("%s: %s:%d: missing %s: %s" % (mode, key[0], key[1], kind, text))
            for kind, message in found:
                if kind != "note":
                    failures += 1
                    print("%s: %s:%d: unexpected %s: %s" % (mode, key[0], key[1], kind, message))
        if verbose:
            print("%s: %d expectations checked" % (mode, checked))
    return failures


def gnat_findings(directory, spans):
    """Compile every body in directory with GNAT and return the statements with dimension errors and literal
    warnings."""
    out = TESTS / "out" / "gnat_objects"
    shutil.rmtree(out, ignore_errors=True)
    out.mkdir(parents=True)
    findings = collections.defaultdict(set)
    for source in sorted(directory.glob("*.adb")):
        result = subprocess.run(
            ["gcc", "-c", "-gnatc", "-gnatf", "-gnat2022", "-gnatW8", "-I" + str(directory), str(source)],
            cwd=out,
            capture_output=True,
            text=True,
        )
        if "compilation abandoned" in result.stderr and "--! GNAT abandons" not in source.read_text(encoding="utf-8"):
            print("GNAT: abandoned the compilation of %s, so later lines are not compared; split the test" % source.name)
            findings[(source.name, 0)].add("abandoned")
        for line in result.stderr.splitlines():
            match = re.match(r"^(?P<file>[^:]+):(?P<line>\d+):\d+: (?P<kind>error|warning): (?P<message>.*)$", line)
            if not match:
                continue
            if match["kind"] == "warning" and "assumed to be" not in match["message"]:
                continue
            if match["kind"] == "error" and "dimension" not in match["message"] and "operand" not in match["message"]:
                print("GNAT: unexpected error: " + line)
                continue
            name, number = Path(match["file"]).name, int(match["line"])
            findings[(name, spans.get(name, {}).get(number, number))].add(match["kind"])
    return findings


def check_gnat_parity(verbose):
    if shutil.which("gcc") is None:
        print("GNAT parity: gcc not found, run this script with `alr exec -- tests/run_tests.py`")
        return 1

    source_dir = TESTS / "gnat"
    migrated = TESTS / "out" / "gnat"
    shutil.rmtree(migrated, ignore_errors=True)
    migrated.mkdir(parents=True)
    for source in source_dir.glob("*.ad[sb]"):
        shutil.copy(source, migrated / source.name)
    subprocess.run(
        [sys.executable, str(MIGRATE), "--quiet", *[str(p) for p in sorted(migrated.glob("*.ad[sb]"))]], check=True
    )
    (migrated / "gnat.gpr").write_text(
        'project GNAT is\n   for Source_Dirs use (".");\n   for Object_Dir use "obj";\nend GNAT;\n', encoding="utf-8"
    )

    spans = spans_of(source_dir)
    expected = gnat_findings(source_dir, spans)
    gnat_false_positives = {
        (source.name, spans[source.name][number])
        for source in source_dir.glob("*.ad[sb]")
        for number, text in enumerate(source.read_text(encoding="utf-8").splitlines(), start=1)
        if "--! GNAT false positive" in text
    }
    failures = 0
    for gnat_compatible in (True, False):
        mode = "--gnat-compatible" if gnat_compatible else "default"
        actual_findings = run_tool(
            migrated / "gnat.gpr", ["--gnat-compatible"] if gnat_compatible else [], CRATE, spans
        )
        actual = {
            key: {kind for kind, _ in found if kind != "note"}
            for key, found in actual_findings.items()
            if any(kind != "note" for kind, _ in found)
        }
        for key in sorted(set(actual) | set(expected)):
            gnat = expected.get(key, set())
            tool = actual.get(key, set())
            if key in gnat_false_positives:
                if gnat and not tool:
                    continue
                print("GNAT parity: %s:%d is marked as a GNAT false positive, but GNAT reports %s and dimension_check %s"
                      % (key[0], key[1], sorted(gnat) or "nothing", sorted(tool) or "nothing"))
                failures += 1
                continue
            if gnat_compatible and gnat != tool or not gnat_compatible and not gnat <= tool:
                failures += 1
                print(
                    "GNAT parity (%s): %s:%d: GNAT reports %s, dimension_check reports %s%s"
                    % (
                        mode,
                        key[0],
                        key[1],
                        sorted(gnat) or "nothing",
                        sorted(tool) or "nothing",
                        "".join("\n    " + message for _, message in actual_findings.get(key, [])),
                    )
                )
        if verbose:
            print("GNAT parity (%s): %d statements with findings compared" % (mode, len(set(actual) | set(expected))))
    return failures


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("-v", "--verbose", action="store_true")
    args = parser.parse_args()

    if not TOOL.exists():
        print("%s not found, build the crate first" % TOOL)
        return 1

    (TESTS / "out" / "obj").mkdir(parents=True, exist_ok=True)
    failures = check_expectations(args.verbose) + check_gnat_parity(args.verbose)
    print("%s: %d failure(s)" % ("FAILED" if failures else "PASSED", failures))
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
