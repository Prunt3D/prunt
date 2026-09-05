Run focused tests using `PRUNT_SKIP_VALIDATION=true PRUNT_XCOV_DUMP=false PRUNT_SKIP_INTEGRATION=true ./run_tests.sh <filter>`. The final optional argument filters both unit tests and integration scenarios by substring, (`thermistors` to run `*thermistors*` for example).

Integration tests and coverage can take a long time and should not be run by default. Avoid integration tests by setting `PRUNT_SKIP_INTEGRATION=true`; omit it or set it to `false` only when integration tests are explicitly required. Avoid coverage by keeping `PRUNT_XCOV_DUMP=false` and leaving `PRUNT_DO_GNATCOV` unset or set to `false`; only set `PRUNT_XCOV_DUMP=true` or `PRUNT_DO_GNATCOV=true` when coverage is explicitly required.

Perform a build using `alr build`. You do not need to build before running tests, it happens as part of the test runner.

Run `alr build` and `./run_tests.sh` sequentially, never concurrently in the same checkout. The main build and test build share build artifacts, so overlapping them can cause linker failures.
