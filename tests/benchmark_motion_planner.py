#!/usr/bin/env python3
r"""Compare two release builds with alternating CPU-time trials and exact output checks.

Build corner_planner_time, helix_planner_time and benchy_planner_time with:
  cd tests && alr build --release --profiles=prunt=release -- -j4 -s \
    corner_planner_time.adb helix_planner_time.adb benchy_planner_time.adb
Copy those executables to a baseline directory before changing the planner, then run:
  python3 tests/benchmark_motion_planner.py --baseline /tmp/planner-before \
    --candidate tests/bin --output benchmark_results/planner.json

The perimeter fixture is synthetic, not Benchy. It has 4,000 curved extrusion moves
followed by short collinear moves and reversals, with the counts expected by the
existing Benchy driver. Its continuous case exercises long timing runs.
Every driver prints its segment and block counts, total planned time and a bit-level checksum of every
segment time, which the output checks compare exactly; build the baseline from the same driver sources.
No driver blends blocks: the corner driver plans one block, and the others keep block boundaries stopped
because whether a blend is accepted depends on consumer timing. Only the planner's tests cover inline blending.
Run benchmarks without concurrent builds or tests. CPU time includes process startup.
Use --allow-motion-changes to compare different valid planners: segment/block counts
must still match, while motion duration changes and checksum differences are reported.
This mode measures changes; correctness still requires the planner's numerical tests.
"""

import argparse
import hashlib
import json
import math
import platform
import resource
import statistics
import subprocess
import tempfile
import time
from pathlib import Path


def make_fixture(path):
    lines = ["M83", "G1 X150 Y100 Z0.3 F9000"]
    x, y = 150.0, 100.0
    for i in range(1, 4001):
        angle = i * 0.08
        radius = 50 + 5 * math.sin(i * 0.031)
        nx, ny = 100 + radius * math.cos(angle), 100 + radius * math.sin(angle)
        length = math.hypot(nx - x, ny - y)
        lines.append(f"G1 X{nx:.6f} Y{ny:.6f} E{length * 0.04:.6f} F{6000 + (i % 7) * 1000}")
        x, y = nx, ny
    lines.append("G1 X200 Y100 E0.5 F9000")
    for i in range(47924 - 4002):
        lines.append(f"G1 X{200 + (i % 1000) * 0.04:.6f} Y100 E0.0016 F9000")
    lines.extend(["G1 E0"] * 725)
    path.write_text("\n".join(lines) + "\n")


def measure(directory, executable, arguments):
    command = [str(directory.resolve() / executable), *arguments]
    before = resource.getrusage(resource.RUSAGE_CHILDREN)
    started = time.perf_counter()
    result = subprocess.run(command, capture_output=True, text=True, check=True, timeout=300)
    wall = time.perf_counter() - started
    after = resource.getrusage(resource.RUSAGE_CHILDREN)
    return {
        "cpu_seconds": after.ru_utime + after.ru_stime - before.ru_utime - before.ru_stime,
        "wall_seconds": wall,
        "stdout": result.stdout,
        "stderr": result.stderr,
    }


def fingerprint(output):
    return [line for line in output.splitlines() if not line.startswith("planning_wall_seconds=")]


def structure(output):
    return [
        line for line in fingerprint(output)
        if not line.startswith(("total_planned_seconds=", "segment_time_checksum="))
    ]


def motion_seconds(output):
    for line in output.splitlines():
        if line.startswith("total_planned_seconds="):
            value = float(line.split("=", 1)[1])
            if math.isfinite(value) and value > 0:
                return value
    raise ValueError("benchmark did not report a finite positive motion duration")


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--baseline", type=Path, required=True)
    parser.add_argument("--candidate", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--repeats", type=int, default=5)
    parser.add_argument("--case", action="append", dest="selected")
    parser.add_argument("--allow-motion-changes", action="store_true")
    args = parser.parse_args()
    if args.repeats < 1:
        parser.error("--repeats must be positive")
    args.output.parent.mkdir(parents=True, exist_ok=True)
    report = {"platform": platform.platform(), "executables": {}, "trials": [], "summary": {}}
    report["motion_changes_allowed"] = args.allow_motion_changes
    with tempfile.TemporaryDirectory(prefix="prunt-benchmark-") as temporary:
        fixture = Path(temporary) / "perimeters.gcode"
        make_fixture(fixture)
        report["fixture_sha256"] = hashlib.sha256(fixture.read_bytes()).hexdigest()
        cases = {
            "stopped": ("corner_planner_time", ["0", "2000"]),
            "corners": ("corner_planner_time", ["0.02", "2000"]),
            "extruding_corners": ("corner_planner_time", ["0.02", "2000", "10", "0.04"]),
            "helix": ("helix_planner_time", ["STEREOGRAPHIC_CASE", "1000"]),
            "steep_helix": ("helix_planner_time", ["STEREOGRAPHIC_CASE", "1000", "0.04", "STEEP"]),
            "circular_helix": ("helix_planner_time", ["CIRCULAR_CASE", "1000"]),
            "biarc_helix": ("helix_planner_time", ["BIARC_CASE", "1000"]),
            "perimeters": ("benchy_planner_time", ["BASELINE", str(fixture)]),
            "varied_width": ("benchy_planner_time", ["VARIED_WIDTH_SMOOTH", str(fixture)]),
            "continuous": ("benchy_planner_time", ["CONTINUOUS", str(fixture)]),
        }
        unknown = set(args.selected or ()) - cases.keys()
        if unknown:
            parser.error(f"unknown cases: {', '.join(sorted(unknown))}; choose from {', '.join(cases)}")
        if args.selected:
            cases = {name: case for name, case in cases.items() if name in args.selected}
        for build in ("baseline", "candidate"):
            directory = getattr(args, build).resolve()
            report["executables"][build] = {
                name: {
                    "path": str(directory / name),
                    "sha256": hashlib.sha256((directory / name).read_bytes()).hexdigest(),
                }
                for name in sorted({executable for executable, _ in cases.values()})
            }
        expected = {}
        expected_structure = {}
        expected_per_build = {}
        for repeat in range(args.repeats):
            for name, (executable, arguments) in cases.items():
                order = ("baseline", "candidate") if repeat % 2 == 0 else ("candidate", "baseline")
                for build in order:
                    trial = measure(getattr(args, build), executable, arguments)
                    actual = fingerprint(trial["stdout"])
                    expected.setdefault(name, actual)
                    expected_structure.setdefault(name, structure(trial["stdout"]))
                    expected_per_build.setdefault((name, build), actual)
                    trial["motion_seconds"] = motion_seconds(trial["stdout"])
                    trial.update(case=name, build=build, repeat=repeat, output_matches=actual == expected[name])
                    report["trials"].append(trial)
                    args.output.write_text(json.dumps(report, indent=2) + "\n")
                    if structure(trial["stdout"]) != expected_structure[name]:
                        raise RuntimeError(f"{name}: planned structure changed; see {args.output}")
                    if actual != expected_per_build[name, build]:
                        raise RuntimeError(f"{name}: output is not deterministic; see {args.output}")
                    if not trial["output_matches"] and not args.allow_motion_changes:
                        raise RuntimeError(f"{name}: planned output changed; see {args.output}")
                    print(f"{name:20} {build:9} {trial['cpu_seconds']:.6f} CPU seconds", flush=True)
        for name in expected:
            timings = {
                build: statistics.median(
                    t["cpu_seconds"] for t in report["trials"] if t["case"] == name and t["build"] == build
                )
                for build in ("baseline", "candidate")
            }
            timings["cpu_reduction_percent"] = 100 * (1 - timings["candidate"] / timings["baseline"])
            motions = {
                build: statistics.median(
                    t["motion_seconds"] for t in report["trials"] if t["case"] == name and t["build"] == build
                )
                for build in ("baseline", "candidate")
            }
            timings["motion_duration_change_percent"] = 100 * (motions["candidate"] / motions["baseline"] - 1)
            report["summary"][name] = timings
        args.output.write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps(report["summary"], indent=2))


if __name__ == "__main__":
    main()
