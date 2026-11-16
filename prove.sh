#!/bin/bash
# Verify Prunt's SPARK units with GNATprove: the feedrate profile generator (Prunt.Motion_Planner.Trajectory), the
# stereographic corner blender (Prunt.Motion_Planner.Stereographic_Curves and its Geometry child, with the proved
# kernel Prunt.Motion_Planner.Stereographic_Kernel), the real-number model both use (Prunt.Motion_Planner.Reals) and
# the eigenvalue solver which finds the blender's poles (Prunt.LAPACK). In two steps:
#
#   1. flow analysis of every SPARK unit (SPARK legality, initialization, aliasing, termination);
#   2. proof of every SPARK unit. In Stereographic_Curves the subprograms which recover from overflow by handling
#      Constraint_Error carry the annotation Skip_Proof and are not proved.
#
# docs/spark_trajectory.md, docs/spark_blender.md and docs/spark_lapack.md say what is proved and what is assumed.
# Each step prints its unproved checks, if any, and the totals so far. The whole output is kept in
# obj/gnatprove-run.txt.
#
# Each step runs through ./gnatprove.sh, which starts the prover server itself so that the number of provers and
# the number of gnatwhy3 processes are set separately (PROVE_PROVERS and PROVE_LANES, described there). Level 2
# gives each prover a few seconds of wall-clock time, so on a busy machine lower PROVE_PROVERS.
#
# Without -f, only changed units are analysed again, and in them every goal which an earlier run settled is taken
# from its session in obj/development/gnatprove instead of being proved again, also when an edit only moved it to
# other lines. A goal which failed is retried only with more provers or a longer timeout than before (a higher
# --level, or --timeout). Proofs are shared with the other checkouts of Prunt too, through ~/.cache/prunt/gnatprove
# (gnatprove.sh describes how), so a new worktree takes what another has proved. Pass -f for a run which reuses
# nothing. Other arguments are passed to GNATprove too.
set -eo pipefail
cd "$(dirname "$0")"

# Configure Prunt without the test mocks, regenerate its generated sources and build SPARKlib, whose build directory
# GNATprove needs.
alr -n build

units=""
for spec in src/prunt-motion_planner-reals.ads src/prunt-motion_planner-trajectory*.ads \
   src/prunt-motion_planner-stereographic_*.ads src/prunt-lapack*.ads; do
   body=${spec%.ads}.adb
   if [ -f "$body" ]; then
      units="$units ${body#src/}"
   else
      units="$units ${spec#src/}"
   fi
done

log=obj/gnatprove-run.txt
mkdir -p obj

report () {
   grep -E "medium|high|error|Success" || true
   sed -n '/^SPARK Analysis results/,/^Total/p' obj/development/gnatprove/gnatprove.out
}

echo "== 1. Flow analysis of all SPARK units"
# shellcheck disable=SC2086
./gnatprove.sh --mode=flow --output=oneline "$@" $units 2>&1 | tee "$log" | report

echo "== 2. Proof"
# shellcheck disable=SC2086
./gnatprove.sh --output=oneline "$@" $units 2>&1 | tee -a "$log" | report
grep "Create_Blend at" obj/development/gnatprove/gnatprove.out
