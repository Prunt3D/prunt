#!/usr/bin/env sh

set -e

# Build the project using the validation build profile to enforce static analysis and coding style.
[ "$PRUNT_SKIP_VALIDATION" = true ] || alr build --validation

cd tests

# Exercise the same checks and optimization as production when requested.
PRUNT_TEST_PROFILE=${PRUNT_TEST_PROFILE:-development}
case "$PRUNT_TEST_PROFILE" in
   development|release) ;;
   *) printf '%s\n' 'PRUNT_TEST_PROFILE must be development or release' >&2; exit 1 ;;
esac

build_tests() {
   if [ -n "${PRUNT_TEST_RUNTIME:-}" ]; then
      alr build --"$PRUNT_TEST_PROFILE" --profiles=prunt="$PRUNT_TEST_PROFILE" -- --RTS="$PRUNT_TEST_RUNTIME" "$@"
   else
      alr build --"$PRUNT_TEST_PROFILE" --profiles=prunt="$PRUNT_TEST_PROFILE" -- "$@"
   fi
}

[ "$PRUNT_DO_GNATCOV" = true ] && alr exec -- gnatcov setup --prefix=./gnatcov_rts

# The test suite needs a lot of cleaning up before validation works
# [ "$PRUNT_SKIP_VALIDATION" = true ] || alr build --validation

# Instrument the project code for coverage analysis.
[ "$PRUNT_DO_GNATCOV" = true ] && GPR_PROJECT_PATH="$GPR_PROJECT_PATH:./gnatcov_rts/share/gpr" alr exec -P2 -- gnatcov instrument --level=stmt+mcdc+gexpr --dump-trigger=manual --projects=prunt.gpr --projects=tests.gpr --no-subprojects --ada=2022

# Build testsuite with instrumented code.
[ "$PRUNT_DO_GNATCOV" = true ] && GPR_PROJECT_PATH="$GPR_PROJECT_PATH:./gnatcov_rts/share/gpr" build_tests --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr
[ "$PRUNT_DO_GNATCOV" = true ] || build_tests

rm -f gnatcov_out/*.xcov
rm -f gnatcov_out/*.html
rm -f gnatcov_out/*.css
rm -f *.srctrace

# Run the instrumented testsuite. This will produce at least one .srctrace file for the coverage analysis.
[ "$PRUNT_XCOV_DUMP" = true ] && alr exec ./bin/tests -- xcov_dump "$@"
[ "$PRUNT_XCOV_DUMP" = true ] || alr exec ./bin/tests -- "$@"

if [ "$PRUNT_SKIP_INTEGRATION" != true ]; then
   PRUNT_INTEGRATION_JOBS=${PRUNT_INTEGRATION_JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || printf '1')}
   case "$PRUNT_INTEGRATION_JOBS" in
      ''|*[!0-9]*|0) PRUNT_INTEGRATION_JOBS=1 ;;
   esac
   export PRUNT_XCOV_DUMP

   integration_scenarios=$(alr exec ./bin/integration_tests -- --list "$@")
   if [ -n "$integration_scenarios" ]; then
      printf '%s\n' "$integration_scenarios" |
         xargs -n 1 -P "$PRUNT_INTEGRATION_JOBS" sh -c '
            scenario=$1

            if [ "$PRUNT_XCOV_DUMP" = true ]; then
               alr exec ./bin/integration_scenario_runner -- xcov_dump --scenario="$scenario" --prunt-web-server-port=0
            else
               alr exec ./bin/integration_scenario_runner -- --scenario="$scenario" --prunt-web-server-port=0
            fi
         ' sh
   fi
fi

# Run the GNATcov code coverage analysis on the trace files.
# tests.gpr can be added below to check that all tests are being run.
[ "$PRUNT_DO_GNATCOV" = true ] && alr exec -P2 -- gnatcov coverage --annotate=report --output-dir=gnatcov_out --level=stmt+mcdc+gexpr --projects=prunt.gpr --no-subprojects *.srctrace
[ "$PRUNT_DO_GNATCOV" = true ] && alr exec -P2 -- gnatcov coverage --annotate=shtml+ --output-dir=gnatcov_out --level=stmt+mcdc+gexpr --projects=prunt.gpr --no-subprojects *.srctrace
[ "$PRUNT_DO_GNATCOV" = true ] && alr exec -P2 -- gnatcov coverage --annotate=xcov+ --output-dir=gnatcov_out --level=stmt+mcdc+gexpr --projects=prunt.gpr --no-subprojects *.srctrace
# alr exec -P2 -- gnatcov coverage --annotate=sarif --output-dir=gnatcov_out --level=stmt+mcdc+gexpr --projects=prunt.gpr --no-subprojects *.srctrace
# alr exec -P2 -- gnatcov coverage --annotate=cobertura --output-dir=gnatcov_out --level=stmt+mcdc+gexpr --projects=prunt.gpr --no-subprojects *.srctrace

[ "$PRUNT_XCOV_DUMP" = true ] && alr exec -P2 -- python3 generate_coverage.py

exit 0
