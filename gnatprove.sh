#!/bin/bash
# Run GNATprove on prunt.gpr, as `alr exec -- gnatprove -P prunt.gpr --no-subprojects` would, but with the prover
# server started here, so that the number of prover processes and the number of gnatwhy3 processes are set
# separately. prove.sh runs the whole verification through this script; use it directly for single units, as in
#
#    ./gnatprove.sh --output=oneline prunt-motion_planner-trajectory-solvers.adb
#
# Name the files of the SPARK units to analyse. Without them GNATprove analyses every unit of Prunt, most of which are
# not in SPARK. Do not add -u: --no-subprojects keeps GNATprove out of the dependencies, some of which it cannot
# analyse (gnat2why crashes on the XML units of vss_extra), but only when -u is not given. With -u its generation of
# Global contracts compiles the dependencies' units too.
#
# `gnatprove -jN` translates up to N units at once and runs up to N gnatwhy3 processes (each reads one entity's Why3
# input, splits and transforms its goals and sends them to the prover server) and N prover processes. Level 2 gives
# each prover a few seconds of wall-clock time, so on a busy machine too many provers make proofs time out which
# would succeed, while gnatwhy3 processes are what costs memory. Here up to PROVE_PROVERS prover processes and
# PROVE_LANES gnatwhy3 processes run at once.
#
# Usage: ./gnatprove.sh <arguments of gnatprove, without -P and -j>
#
#   PROVE_PROVERS  prover processes at once (default: three quarters of the logical processors). This must leave
#                  headroom for the provers' wall-clock timeouts.
#   PROVE_LANES    gnatwhy3 processes, and units translated, at once (default: three quarters of the logical
#                  processors).
set -eu
cd "$(dirname "$0")"

for arg in "$@"; do
   case $arg in
      -j*) echo "gnatprove.sh: do not pass -j; set PROVE_LANES and PROVE_PROVERS instead" >&2; exit 2 ;;
      -P*) echo "gnatprove.sh: the project is prunt.gpr" >&2; exit 2 ;;
   esac
done

cpus=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)
default=$(( cpus * 3 / 4 > 0 ? cpus * 3 / 4 : 1 ))
provers=${PROVE_PROVERS:-$default}
lanes=${PROVE_LANES:-$default}

# The proof sessions which let a run reuse earlier results live in obj/<build profile>/gnatprove, and the profile is
# whichever Alire configured last. The tests' build configures Prunt with its mocked sources, which are analysed in
# other directories; prove.sh runs `alr build` first, which configures it without them.
profile=$(sed -n 's/^ *Build_Profile : Build_Profile_Kind := "\(.*\)";/\1/p' config/prunt_config.gpr)
if grep -q 'Enable_Test_Mocks := "True"' config/prunt_config.gpr; then
   echo "gnatprove.sh: Prunt is configured with the test mocks; run \`alr build\` first" >&2
   exit 1
fi
if [ "$profile" != development ]; then
   echo "gnatprove.sh: proving with the $profile profile, whose sessions are not those of the development" \
        "profile; run \`alr build\` first to reuse them" >&2
fi

bin="$(alr -n exec -- sh -c 'dirname "$(command -v gnatprove)"')/../libexec/spark/bin"

# GNATprove learns the layout of types by compiling with -gnatR, with the compiler it is installed with unless
# GNSA_ROOT names another. Give it Prunt's own: gnatprove 16.2.1001 comes with the compiler of gnat_native 16.2.1008,
# whose -gnatR fails an assertion on two ghost types of Prunt.Motion_Planner.Stereographic_Kernel, which 16.2.1009
# fixed. GNATprove still runs its own gprbuild, which gnat_native does not have.
if [ -z "${GNSA_ROOT:-}" ]; then
   GNSA_ROOT="$(alr -n exec -- sh -c 'dirname "$(dirname "$(command -v gnatls)")"')"
   export GNSA_ROOT
fi

sock=$(mktemp -u "${TMPDIR:-/tmp}/prunt_why3server.XXXXXX")

# The server finds the provers on its PATH, as it does when gnatprove starts it.
PATH="$bin:$PATH" "$bin/why3server" -j "$provers" --socket "$sock" &
server=$!
trap 'kill "$server" 2>/dev/null || true; rm -f "$sock"' EXIT
for _ in $(seq 50); do
   [ -S "$sock" ] && break
   sleep 0.1
done

alr -n exec -- gnatprove -P prunt.gpr --no-subprojects "$@" -j"$lanes" --why3-server="$sock"
