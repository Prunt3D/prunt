Configuration persistence checks
================================

Run the focused configuration unit tests with:

```
PRUNT_SKIP_VALIDATION=true PRUNT_XCOV_DUMP=false PRUNT_SKIP_INTEGRATION=true ./run_tests.sh config
```

The tests interrupt initialization and saves at each mocked filesystem mutation
and synchronization boundary. The save sweep fills all 20 backups first, so it
also covers deletion and rotation. Ordinary IO failures preserve pending deltas
for retry. Simulated power loss discards unsynced data and directory changes;
reopening must produce a complete old or new configuration. Successful saves
must retain the new configuration. Separate regressions cover copying, writer
lifetimes, reset, validation failures, and failed patches.

To test the production filesystem backend independently:

```
alr exec -- python3 tests/config_persistence_io.py
```

This compiles the actual persistence backend with minimal parent packages in a
temporary directory. It kills writer subprocesses at nine boundaries, including
a partially written temporary file and either side of atomic replacement. It
also checks cross-process locking through relative paths and symlinks, lock
release after process death, and rejection of hard-linked files.

SIGKILL tests process interruption; the mocked durable-state tests simulate power
loss. Neither substitutes for power-cut testing of a particular device,
filesystem, and storage controller. Durability requires working file and
directory fsync support. A failed sync after replacement can leave the new file
on disk even though Save reports failure; its in-memory deltas remain retryable.

The persistent `.lock` sidecar must not be removed while writers could exist.
Temporary `.tmp` files left by interruption are ignored on open and overwritten
by the next save. Finalization deliberately discards unsaved changes, consistent
with the explicit Save API. Backup rotation is best effort across interruption;
the primary configuration is the atomic, durable commit point.
