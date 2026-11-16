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
# Proofs are shared between the checkouts of Prunt (its worktrees), so that one does not prove again what another
# has proved. Two stores in PROVE_CACHE, kept apart for each GNATprove release, do this:
#
#   results   GNATprove's file cache (--memcached-server=file:DIR). It keeps the output of each run of gnatwhy3 and
#             of each prover call under a hash of its exact input, so it serves an entity (a subprogram or package)
#             only when its Why3 input is unchanged, in whichever checkout. An edit which moves code to other lines
#             changes the input of every entity after it. For an entity which the cache serves gnatwhy3 does not run
#             and writes no session, so the shared sessions below are what reuses its proofs after such an edit.
#             The output of a run of gnatwhy3 which left a check unproved is deleted again, so that a failure,
#             perhaps a timeout on a loaded machine, is handled as it would be without the cache: a later run asks
#             gnatwhy3, which retries the check with more provers or a longer timeout than before.
#   sessions  the latest session of each entity, from whichever checkout last changed it. Before a run, every
#             entity without a session in this checkout is given that one. After a run, every session which
#             gnatwhy3 changed replaces it. Sessions written while another run reads them are not torn: each is one
#             tar file, replaced by renaming.
#
# A session lets gnatwhy3 take a goal which an earlier run proved, also after an edit which moved it to other lines.
# GNATprove keeps the sessions in obj/<build profile>/gnatprove. With -f nothing is taken from either store or put
# into them: -f is for a run which reuses nothing, and it starts each entity from an empty session, which after
# -f --limit-line holds the proofs of that line's goals only.
#
# Usage: ./gnatprove.sh <arguments of gnatprove, without -P and -j>
#
#   PROVE_PROVERS     prover processes at once (default: three quarters of the logical processors). This must leave
#                     headroom for the provers' wall-clock timeouts.
#   PROVE_LANES       gnatwhy3 processes, and units translated, at once (default: three quarters of the logical
#                     processors).
#   PROVE_CACHE       directory of the stores shared between checkouts (default: ~/.cache/prunt/gnatprove, or
#                     prunt/gnatprove in XDG_CACHE_HOME, beside the caches of prebuild). "off" shares nothing.
#   PROVE_CACHE_DAYS  a file of PROVE_CACHE which no run has read for this many days is deleted (default: 30).
set -eu
cd "$(dirname "$0")"

force=false
for arg in "$@"; do
   case $arg in
      -j*) echo "gnatprove.sh: do not pass -j; set PROVE_LANES and PROVE_PROVERS instead" >&2; exit 2 ;;
      -P*) echo "gnatprove.sh: the project is prunt.gpr" >&2; exit 2 ;;
      -f) force=true ;;
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

sessions=obj/$profile/gnatprove

# The GNATprove release, such as .../gnatprove_16.2.1001_0dc82278.
prefix="$(alr -n exec -- sh -c 'dirname "$(dirname "$(command -v gnatprove)")"')"
bin=$prefix/libexec/spark/bin

# The stores shared between checkouts, described above. The file cache's key covers neither gnatwhy3 nor the
# provers, so every GNATprove release has stores of its own. The cache must be named by an absolute path: with a
# relative one it reads entries but cannot write them.
cache=""
cache_args=()
root=${PROVE_CACHE:-${XDG_CACHE_HOME:-$HOME/.cache}/prunt/gnatprove}
if [ "$root" != off ] && ! $force; then
   mkdir -p "$root"
   root=$(realpath "$root")
   # At most once a day, delete what no run has read for PROVE_CACHE_DAYS days: entries of code which has changed
   # since, sessions of entities which no longer exist, and in time the stores of earlier GNATprove releases.
   if [ -z "$(find "$root/.pruned" -mmin -1440 2>/dev/null)" ]; then
      touch "$root/.pruned"
      find "$root" -mindepth 1 -type f -atime +"${PROVE_CACHE_DAYS:-30}" -delete
      find "$root" -mindepth 1 -type d -empty -delete
   fi
   cache=$root/$(basename "$prefix")
   mkdir -p "$cache/results" "$cache/sessions" "$sessions"
   cache_args=(--memcached-server="file:$cache/results")
fi

# Give every entity which has no session here the latest one of the shared store. They are unpacked aside and then
# moved into place, so that an interrupted run leaves no session without its shapes.
seed_sessions () {
   local tar entity tmp
   local missing=()
   for tar in "$cache/sessions"/*.tar; do
      [ -e "$tar" ] || continue
      entity=${tar##*/}
      [ -e "$sessions/${entity%.tar}" ] || missing+=("$tar")
   done
   [ ${#missing[@]} -gt 0 ] || return 0
   rm -rf "$sessions"/.seed.*
   tmp=$(mktemp -d "$sessions/.seed.XXXXXX")
   printf '%s\0' "${missing[@]}" | xargs -0 cat | tar -x -i -f - -C "$tmp"
   find "$tmp" -mindepth 1 -maxdepth 1 -exec mv -t "$sessions" {} +
   rm -rf "$tmp"
}

# Put every session which gnatwhy3 wrote since it was last put into the shared store there. A session given by
# seed_sessions keeps the time at which it was written, which is earlier than the store's copy.
publish_sessions () {
   local xml entity tmp
   for xml in "$sessions"/*/why3session.xml; do
      [ -e "$xml" ] || continue
      entity=${xml%/why3session.xml}
      entity=${entity##*/}
      [ "$xml" -nt "$cache/sessions/$entity.tar" ] || continue
      tmp=$(mktemp "$cache/sessions/.$entity.XXXXXX")
      tar -c -f "$tmp" -C "$sessions" --exclude='*.bak' "$entity"
      mv -f "$tmp" "$cache/sessions/$entity.tar"
   done
}

# Delete the outputs of gnatwhy3 which this run (or another one at the same time) put into the cache and which leave
# a check unproved. GNATprove's cache keeps every output of gnatwhy3 which exits normally, and gnatwhy3 exits
# normally when it could not prove a check.
forget_failures () {
   find "$cache/results" -type f -newer "$1" -exec grep -l -F '"result":false' {} + | xargs -r rm -f || true
}

# GNATprove learns the layout of types by compiling with -gnatR, with the compiler it is installed with unless
# GNSA_ROOT names another. Give it Prunt's own: gnatprove 16.2.1001 comes with the compiler of gnat_native 16.2.1008,
# whose -gnatR fails an assertion on two ghost types of Prunt.Motion_Planner.Stereographic_Kernel, which 16.2.1009
# fixed. GNATprove still runs its own gprbuild, which gnat_native does not have.
if [ -z "${GNSA_ROOT:-}" ]; then
   GNSA_ROOT="$(alr -n exec -- sh -c 'dirname "$(dirname "$(command -v gnatls)")"')"
   export GNSA_ROOT
fi

sock=$(mktemp -u "${TMPDIR:-/tmp}/prunt_why3server.XXXXXX")
start=""

# The server finds the provers on its PATH, as it does when gnatprove starts it.
PATH="$bin:$PATH" "$bin/why3server" -j "$provers" --socket "$sock" &
server=$!
trap 'kill "$server" 2>/dev/null || true; rm -f "$sock" ${start:+"$start"}' EXIT
for _ in $(seq 50); do
   [ -S "$sock" ] && break
   sleep 0.1
done

if [ -n "$cache" ]; then
   seed_sessions
   start=$(mktemp "$cache/.run.XXXXXX")
fi

status=0
alr -n exec -- gnatprove -P prunt.gpr --no-subprojects "${cache_args[@]}" "$@" -j"$lanes" --why3-server="$sock" \
   || status=$?

if [ -n "$cache" ]; then
   forget_failures "$start"
   publish_sessions
fi
exit $status
