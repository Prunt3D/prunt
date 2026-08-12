#!/usr/bin/env sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
tests_dir="$script_dir/tests"
build_subdir=benchmark_optimisation_flags
benchmark_executable="$tests_dir/bin/$build_subdir/benchy_planner_time"
timestamp=$(date +%Y%m%d-%H%M%S)
results_dir=${PRUNT_BENCHMARK_RESULTS_DIR:-"$script_dir/benchmark_results/$timestamp"}
results_file="$results_dir/results.tsv"

if [ ! -x /usr/bin/time ]; then
   printf '%s\n' 'error: /usr/bin/time is required to measure each benchmark run' >&2
   exit 1
fi

mkdir -p "$results_dir"
printf 'configuration\tcompiler_flags\telapsed_seconds\texecutable_bytes\trun_log\n' >"$results_file"

benchmark_configuration()
{
   configuration=$1
   compiler_flags=$2
   build_log="$results_dir/$configuration-build.log"
   run_log="$results_dir/$configuration-run.log"
   time_file="$results_dir/$configuration.time"

   printf '\nBuilding %s with: %s\n' "$configuration" "$compiler_flags"
   if ! (
      cd "$tests_dir"
      alr build --profiles=tests=release,prunt=release -- \
         -XPRUNT_BENCHMARK_MODE=enabled \
         "-XPRUNT_BENCHMARK_COMPILER_SWITCHES=$compiler_flags" \
         "--subdirs=$build_subdir" \
         -s benchy_planner_time.adb
   ) >"$build_log" 2>&1; then
      printf 'Build failed; see %s\n' "$build_log" >&2
      printf '%s\t%s\t%s\t%s\t%s\n' \
         "$configuration" "$compiler_flags" "BUILD_FAILED" "-" "$build_log" >>"$results_file"
      return
   fi

   executable_bytes=$(wc -c <"$benchmark_executable" | tr -d ' ')
   printf 'Running %s (one trial)...\n' "$configuration"
   if ! (
      cd "$tests_dir"
      alr exec -- /usr/bin/time -f '%e' -o "$time_file" "./bin/$build_subdir/benchy_planner_time"
   ) >"$run_log" 2>&1; then
      printf 'Benchmark failed; see %s\n' "$run_log" >&2
      printf '%s\t%s\t%s\t%s\t%s\n' \
         "$configuration" "$compiler_flags" "RUN_FAILED" "$executable_bytes" "$run_log" >>"$results_file"
      return
   fi

   elapsed_seconds=$(tr -d '[:space:]' <"$time_file")
   printf '%s: %s seconds\n' "$configuration" "$elapsed_seconds"
   printf '%s\t%s\t%s\t%s\t%s\n' \
      "$configuration" "$compiler_flags" "$elapsed_seconds" "$executable_bytes" "$run_log" >>"$results_file"
}

benchmark_configuration o2                 '-O2'
benchmark_configuration o3                 '-O3'
benchmark_configuration o2-gnatp           '-O2,-gnatp'
benchmark_configuration o3-gnatp           '-O3,-gnatp'
benchmark_configuration o2-lto              '-O2,-flto=auto,-fuse-linker-plugin,-ffat-lto-objects'
benchmark_configuration o3-lto              '-O3,-flto=auto,-fuse-linker-plugin,-ffat-lto-objects'
benchmark_configuration o2-lto-gnatp        '-O2,-gnatp,-flto=auto,-fuse-linker-plugin,-ffat-lto-objects'
benchmark_configuration o3-lto-gnatp        '-O3,-gnatp,-flto=auto,-fuse-linker-plugin,-ffat-lto-objects'

# GCC currently crashes on this project with -gnatn in some configurations.
# benchmark_configuration o2-gnatn1          '-O2,-gnatn1'
# benchmark_configuration o3-gnatn2          '-O3,-gnatn2'

printf '\nCompleted results (fastest first):\n'
awk -F '\t' 'NR > 1 && $3 ~ /^[0-9]+([.][0-9]+)?$/ { print }' "$results_file" |
   sort -t "$(printf '\t')" -k3,3n
printf '\nFull results and logs: %s\n' "$results_dir"
