#!/usr/bin/env python3
"""Format Ada sources with the Ada Language Server's GNATformat, using the Format package of each GPR file.

Every source is governed by the GPR file in its nearest enclosing directory. Each GPR file is loaded as the root
project of its own ALS session, in the Alire environment of its crate, because ALS reads formatting rules from the
root project only. Sources that the project does not contain under the current scenario (the mocked mockable units,
prebuild.adb) are formatted under the name of one of the project's own sources, otherwise ALS would format them with
the default rules.

Usage: ./format_ada.py [--check | --diff] [FILE...]

With no FILE, every tracked .ads and .adb file is formatted. With --check (list the files) or --diff (show the
changes), nothing is written and the exit status is 1 if any file would change. ADA_LANGUAGE_SERVER overrides the
ada_language_server executable found on PATH.
"""

import argparse
import concurrent.futures
import difflib
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent
ALS = os.environ.get("ADA_LANGUAGE_SERVER") or "ada_language_server"


class LanguageServerError(Exception):
    pass


class LanguageServer:
    def __init__(self, project, env, trace_dir):
        self.trace_dir = trace_dir
        self.process = subprocess.Popen(
            [ALS, "--tracefile", trace_dir / "traces.cfg"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            cwd=project.parent,
            env=env,
        )
        self.last_id = 0
        self.messages = []
        self.diagnostics = {}
        self.request(
            "initialize",
            {
                "processId": os.getpid(),
                "rootUri": project.parent.as_uri(),
                "capabilities": {},
                "initializationOptions": {"ada": {"projectFile": str(project), "enableIndexing": False}},
            },
        )
        self.notify("initialized", {})

    def send(self, message):
        body = json.dumps({"jsonrpc": "2.0", **message}).encode()
        self.process.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        self.process.stdin.flush()

    def receive(self):
        length = None
        while (line := self.process.stdout.readline()) not in (b"\r\n", b""):
            name, _, value = line.decode().partition(":")
            if name.lower() == "content-length":
                length = int(value)
        if length is None:
            raise LanguageServerError(f"{ALS} exited unexpectedly")
        return json.loads(self.process.stdout.read(length))

    def notify(self, method, params=None):
        self.send({"method": method} | ({} if params is None else {"params": params}))

    def request(self, method, params=None):
        self.last_id += 1
        self.send({"id": self.last_id, "method": method} | ({} if params is None else {"params": params}))
        while True:
            message = self.receive()
            if "method" not in message:
                if message.get("id") == self.last_id:
                    if "error" in message:
                        raise LanguageServerError(message["error"]["message"])
                    return message.get("result")
            elif "id" in message:
                # Progress and capability registration requests only need an acknowledgement.
                self.send({"id": message["id"], "result": None})
            elif message["method"] == "window/showMessage" and message["params"]["type"] <= 2:
                self.messages.append(message["params"]["message"])
            elif message["method"] == "textDocument/publishDiagnostics":
                self.diagnostics[message["params"]["uri"]] = message["params"]["diagnostics"]

    def project_sources(self, project):
        view = self.request("workspace/executeCommand", {"command": "als-project-view-information", "arguments": []})
        for entry in (view or {}).get("projects", []):
            if Path(entry["project"]["file-name"]) == project:
                return sorted(Path(s["file-name"]) for s in entry.get("sources", []) if s["language"] == "ada")
        raise LanguageServerError("\n".join(["could not load the project", *self.messages]))

    def format(self, uri, text):
        self.diagnostics.pop(uri, None)
        self.notify(
            "textDocument/didOpen", {"textDocument": {"uri": uri, "languageId": "ada", "version": 1, "text": text}}
        )
        try:
            # A zero tab size leaves the indentation to GNATformat's defaults where the GPR file does not set it.
            edits = self.request(
                "textDocument/formatting",
                {"textDocument": {"uri": uri}, "options": {"tabSize": 0, "insertSpaces": True}},
            )
        except LanguageServerError as e:
            syntax_errors = [
                f"{d['range']['start']['line'] + 1}:{d['range']['start']['character'] + 1}: {d['message']}"
                for d in self.diagnostics.get(uri, [])
                if d.get("severity", 1) == 1
            ]
            raise LanguageServerError("\n  ".join([str(e), *syntax_errors])) from None
        finally:
            self.notify("textDocument/didClose", {"textDocument": {"uri": uri}})
        return apply_edits(text, edits or [])

    def layout_errors(self):
        """Errors in the Override_Layout files, which ALS only logs before falling back to the default layout."""
        log = self.trace_dir / f"als.{self.process.pid}.log"
        lines = log.read_text(errors="replace").splitlines() if log.is_file() else []
        return sorted(
            {
                following.removeprefix("[ALS.MAIN] ")
                for line, following in zip(lines, lines[1:])
                if "Diagnostics found while loading the unparsing configuration" in line
            }
        )

    def close(self):
        if self.process.poll() is None:
            try:
                self.request("shutdown")
                self.notify("exit")
                self.process.stdin.close()
                self.process.wait(timeout=10)
            except (LanguageServerError, OSError, subprocess.TimeoutExpired):
                self.process.kill()


def apply_edits(text, edits):
    line_starts = [0] + [m.end() for m in re.finditer(r"\r\n|\r|\n", text)]

    def offset(position):
        # LSP positions count UTF-16 code units from the start of the line.
        if position["line"] >= len(line_starts):
            return len(text)
        index = line_starts[position["line"]]
        units = position["character"]
        while units > 0 and index < len(text) and text[index] not in "\r\n":
            units -= 2 if ord(text[index]) > 0xFFFF else 1
            index += 1
        return index

    spans = [(offset(e["range"]["start"]), offset(e["range"]["end"]), e["newText"]) for e in edits]
    # Splice from the end so earlier offsets stay valid; the stable sort keeps insertions at one position in order.
    for start, end, new_text in reversed(sorted(spans, key=lambda span: span[0])):
        text = text[:start] + new_text + text[end:]
    return text


def alire_environment(project):
    crate = next((d for d in project.parents if (d / "alire.toml").is_file()), None)
    env = dict(os.environ)
    if crate is not None:
        output = subprocess.run(
            ["alr", "-n", "printenv", "--unix"], cwd=crate, capture_output=True, text=True, check=True
        ).stdout
        for line in output.splitlines():
            words = shlex.split(line)
            if len(words) == 2 and words[0] == "export":
                name, _, value = words[1].partition("=")
                env[name] = value
    return env


def git_files(*patterns):
    output = subprocess.run(
        ["git", "ls-files", "-z", "--", *patterns], cwd=ROOT, capture_output=True, text=True, check=True
    ).stdout
    return [ROOT / f for f in output.split("\0") if f and (ROOT / f).is_file()]


def format_project(project, files, trace_dir):
    """Format the files governed by one GPR file, returning ((file, text, formatted text) changes, error messages)."""
    changes, errors = [], []
    server = LanguageServer(project, alire_environment(project), trace_dir)
    try:
        sources = server.project_sources(project)
        for file in files:
            if file in sources:
                uri = file.as_uri()
            elif sources:
                uri = sources[0].as_uri()
            else:
                errors.append(f"{file.relative_to(ROOT)}: {project.name} has no Ada sources to format it as")
                continue
            with open(file, encoding="utf-8", newline="") as f:
                text = f.read()
            try:
                formatted = server.format(uri, text)
            except LanguageServerError as e:
                errors.append(f"{file.relative_to(ROOT)}: {e}")
                continue
            if formatted != text:
                changes.append((file, text, formatted))
    except LanguageServerError as e:
        errors.append(f"{project.relative_to(ROOT)}: {e}")
    finally:
        server.close()
    if layout_errors := server.layout_errors():
        # The files were formatted with the default layout instead, so keep none of the changes.
        changes = []
        errors += [f"{project.relative_to(ROOT)}: Override_Layout: {e}" for e in layout_errors]
    return changes, errors


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--check", action="store_true", help="report files that would change without writing them")
    parser.add_argument("--diff", action="store_true", help="print the changes as a diff without writing them")
    parser.add_argument("files", nargs="*", type=Path, help="files to format (default: all tracked Ada sources)")
    args = parser.parse_args()

    if shutil.which(ALS) is None:
        sys.exit(f"{ALS} not found; put the Ada Language Server on PATH or set ADA_LANGUAGE_SERVER")

    files = [f.resolve() for f in args.files] or git_files("*.ads", "*.adb")
    projects = git_files("*.gpr")

    by_project = {}
    for file in files:
        if not file.is_file():
            sys.exit(f"{file}: no such file")
        governing = [p for p in projects if p.parent in file.parents]
        if not governing:
            sys.exit(f"{file}: no GPR file governs this file")
        by_project.setdefault(max(governing, key=lambda p: len(p.parts)), []).append(file)

    changes, errors = [], []
    with tempfile.TemporaryDirectory() as tmp:
        # Log to a temporary file per session rather than to a new file under ~/.als, keeping only what
        # LanguageServer.layout_errors needs.
        trace_dir = Path(tmp)
        (trace_dir / "traces.cfg").write_text(f">{trace_dir}/als.$$.log\nALS.MAIN=yes\nALS.IN=no\nALS.OUT=no\n")
        with concurrent.futures.ThreadPoolExecutor() as pool:
            jobs = [pool.submit(format_project, p, fs, trace_dir) for p, fs in by_project.items()]
            for job in jobs:
                project_changes, project_errors = job.result()
                changes += project_changes
                errors += project_errors

    dry_run = args.check or args.diff
    for file, text, formatted in sorted(changes):
        name = str(file.relative_to(ROOT))
        if args.diff:
            sys.stdout.writelines(
                difflib.unified_diff(
                    text.splitlines(keepends=True), formatted.splitlines(keepends=True), "a/" + name, "b/" + name
                )
            )
        elif args.check:
            print(f"Would reformat {name}")
        else:
            with open(file, "w", encoding="utf-8", newline="") as f:
                f.write(formatted)
            print(f"Reformatted {name}")
    sys.stdout.flush()
    for error in errors:
        print(f"error: {error}", file=sys.stderr)
    print(f"{len(changes)} of {len(files)} files {'would be ' if dry_run else ''}reformatted", file=sys.stderr)
    sys.exit(1 if errors or (dry_run and changes) else 0)


if __name__ == "__main__":
    main()
