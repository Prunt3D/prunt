#!/usr/bin/env sh

set -e

# Build the project using the validation build profile to enforce static analysis and coding style.
[ "$PRUNT_SKIP_VALIDATION" = true ] || alr build --validation

cd tests

# The test suite needs a lot of cleaning up before validation works
# [ "$PRUNT_SKIP_VALIDATION" = true ] || alr build --validation

# Instrument the project code for coverage analysis.
alr exec -P2 -- /usr/local/gnat/bin/gnatcov instrument --level=stmt+mcdc+gexpr --dump-trigger=manual --projects=prunt.gpr --projects=tests.gpr --no-subprojects --ada=2022

# Build testsuite with instrumented code.
alr build --development -- --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts

rm -f gnatcov_out/*.xcov
rm -f gnatcov_out/*.html
rm -f gnatcov_out/*.css
rm -f *.srctrace

# Run the instrumented testsuite. This will produce at least one `.srctrace` file for the coverage analysis.
[ "$PRUNT_XCOV_DUMP" = true ] && alr exec ./bin/tests -- xcov_dump
[ "$PRUNT_XCOV_DUMP" = true ] || alr exec ./bin/tests

# Run the GNATcov code coverage analysis on the trace files.
# tests.gpr can be added below to check that all tests are being run.
alr exec -P2 -- /usr/local/gnat/bin/gnatcov coverage --annotate=report --output-dir=gnatcov_out --level=stmt+mcdc+gexpr --projects=prunt.gpr --no-subprojects *.srctrace
alr exec -P2 -- /usr/local/gnat/bin/gnatcov coverage --annotate=shtml+ --output-dir=gnatcov_out --level=stmt+mcdc+gexpr --projects=prunt.gpr --no-subprojects *.srctrace
alr exec -P2 -- /usr/local/gnat/bin/gnatcov coverage --annotate=xcov+ --output-dir=gnatcov_out --level=stmt+mcdc+gexpr --projects=prunt.gpr --no-subprojects *.srctrace
# alr exec -P2 -- /usr/local/gnat/bin/gnatcov coverage --annotate=sarif --output-dir=gnatcov_out --level=stmt+mcdc+gexpr --projects=prunt.gpr --no-subprojects *.srctrace
# alr exec -P2 -- /usr/local/gnat/bin/gnatcov coverage --annotate=cobertura --output-dir=gnatcov_out --level=stmt+mcdc+gexpr --projects=prunt.gpr --no-subprojects *.srctrace

[ "$PRUNT_XCOV_DUMP" = true ] && alr exec -P2 -- python3 generate_coverage.py
