#!/usr/bin/env python3
"""Exercise the production POSIX persistence backend in isolated subprocesses.

Run from the repository root: alr exec -- python3 tests/config_persistence_io.py
The config unit tests separately simulate loss of unsynced filesystem state.
"""
import json
from pathlib import Path
import shutil
import select
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[1]
DRIVER = '''with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.IO_Exceptions;
with Prunt.Mockable.Persistence; use Prunt.Mockable.Persistence;
procedure Probe is
   Lease : Writer_Lease;
   File : File_Type;
   procedure Checkpoint is
      Line : String (1 .. 1);
      Last : Natural;
   begin
      Put_Line ("ready");
      Flush;
      Get_Line (Line, Last);
   end Checkpoint;
begin
   Acquire (Lease, Argument (1));
   if Argument_Count > 1 then
      return;
   end if;
   Checkpoint;
   Create (File, Out_File, Name (Lease) & ".tmp");
   Checkpoint;
   Put (File, "{""generation"":");
   Flush (File);
   Checkpoint;
   Put_Line (File, "7}");
   Close (File);
   Checkpoint;
   Sync (Name (Lease) & ".tmp");
   Checkpoint;
   Copy (Name (Lease), Name (Lease) & "_backup_1");
   Checkpoint;
   Sync (Name (Lease) & "_backup_1");
   Checkpoint;
   Replace (Name (Lease) & ".tmp", Name (Lease));
   Checkpoint;
   Sync_Parent (Name (Lease));
   Checkpoint;
exception
   when Ada.IO_Exceptions.Use_Error =>
      Set_Exit_Status (Failure);
end Probe;
'''


def run():
    with tempfile.TemporaryDirectory(prefix="prunt-persistence-") as directory:
        work = Path(directory)
        # Minimal parents isolate the exact production backend from controller code.
        (work / "prunt.ads").write_text("package Prunt is end Prunt;\n")
        (work / "prunt-mockable.ads").write_text(
            "package Prunt.Mockable is end Prunt.Mockable;\n"
        )
        for source in (
            ROOT / "src/prunt-mockable-persistence.ads",
            ROOT / "src/mockable_passthrough/prunt-mockable-persistence.adb",
        ):
            shutil.copy(source, work)
        (work / "probe.adb").write_text(DRIVER)
        subprocess.run(
            ["gcc", "-c", str(ROOT / "src/prunt_config_file_links.c"), "-o", "links.o"],
            cwd=work, check=True, timeout=60,
        )
        subprocess.run(
            ["gnatmake", "-q", "-gnat2022", "probe.adb", "-largs", "links.o"],
            cwd=work, check=True, timeout=60,
        )
        probe = str(work / "probe")
        primary = work / "config.json"
        alias = work / "alias.json"
        alias.symlink_to(primary)
        for stage in range(9):
            primary.write_text('{"generation":3}\n')
            process = subprocess.Popen(
                [probe, str(primary)], cwd=work, stdin=subprocess.PIPE,
                stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
            )
            try:
                for index in range(stage + 1):
                    assert select.select([process.stdout], [], [], 10)[0], (stage, index)
                    assert process.stdout.readline().strip() == "ready"
                    if index < stage:
                        process.stdin.write("\n")
                        process.stdin.flush()
                # The lease must survive replacement of the primary inode.
                for path in (primary, alias, Path("./config.json")):
                    result = subprocess.run([probe, str(path), "lock-only"], cwd=work, timeout=10)
                    assert result.returncode != 0, (stage, path)
                process.kill()
                process.wait(timeout=10)
                value = json.loads(primary.read_text())["generation"]
                assert value == (7 if stage >= 7 else 3), (stage, value)
                # Process death releases the lock without deleting its sidecar.
                subprocess.run([probe, str(primary), "lock-only"], cwd=work, check=True, timeout=10)
            finally:
                if process.poll() is None:
                    process.kill()
                    process.wait(timeout=10)
                process.stdin.close()
                process.stdout.close()
                process.stderr.close()
        hard_link = work / "hard-link.json"
        hard_link.hardlink_to(primary)
        for path in (primary, hard_link):
            assert subprocess.run([probe, str(path), "lock-only"], cwd=work, timeout=10).returncode != 0
        print("PASS: 9 SIGKILL boundaries, cross-process aliases, lock release, hard-link rejection")


if __name__ == "__main__":
    run()
