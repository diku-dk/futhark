#!/bin/sh
#
# Gather data from just the benchmarking, and only in two versions:
#
#   + In-place lowering enabled, memory block merging (both memory coalescing
#     and reuse) disabled.
#   + The other way round.
#
# The first argument should be a directory name.  This script will then create
# that directory and store gathered data in multiple JSON files in a
# subdirectory "runs".
#
# When this script has completed, you can run './merge-data.py' to gather all
# the data into a single JSON file.

set -ex # Exit on first error, be verbose.

result_dir="$1"
if ! [ "$result_dir" ]; then
    echo 'error: specify output directory as first argument' > /dev/stderr
    exit 1
fi

flags=''

compiler="$2"
if ! [ "$compiler" ]; then
    compiler='futhark-c'
fi

flags="$flags --compiler $compiler"

timeout_secs="$3"
if [ "$timeout_secs" ]; then
    # Effectively ignore too large datasets.
    flags="$flags --timeout $timeout_secs"
fi

number_runs="$4"
if [ "$number_runs" ]; then
    # Change the default of 10 runs.
    flags="$flags -r $number_runs"
fi

benchmark_programs_file="$5"
if [ "$benchmark_programs_file" ]; then
    benchmark_programs="$(cat "$benchmark_programs_file")"
else
    benchmark_programs='.'
fi

base="$(readlink -f "$result_dir")"

cd "$(dirname "$0")/../../"
stack install

# Assumes your futhark-benchmarks directory is next to your futhark directory.
cd "../futhark-benchmarks/"

get_compilation_info() {
    if [ "$compiler" = 'futhark-c' ]; then
        target_flag='--cpu'
    else
        target_flag='--gpu'
    fi

    {
        export FUTHARK_DEBUG_JSON=1
        temp_storage="$(mktemp)"
        echo '{'
        find -name '*.fut' | while read file; do
            echo "Getting compilation information for $file." > /dev/stderr
            futhark $target_flag $file 2> /dev/null > "$temp_storage" && \
                {
                    echo ','
                    echo '"'"$file"'":'
                    echo '['
                    cat "$temp_storage" | while read line; do
                        echo ','
                        echo "$line"
                    done | tail -n +2 # Requires GNU tail.
                    echo ']'
                } || true
        done | tail -n +2
        echo '}'
    }
}

mkdir "$base"
base="$base/runs"
mkdir "$base"

# Get compilation information and runtime measurements.
get_compilation_info > \
                     "$base/compilation_without-coalescing_without-reuse.json"
futhark-bench $flags --json \
              "$base/measurements_without-coalescing_without-reuse.json" $benchmark_programs \
    || true

export IN_PLACE_LOWERING=0
export MEMORY_BLOCK_MERGING_COALESCING=1
export MEMORY_BLOCK_MERGING_REUSE=1
get_compilation_info > \
                     "$base/compilation_with-coalescing_with-reuse.json"
futhark-bench $flags --json \
              "$base/measurements_with-coalescing_with-reuse.json" $benchmark_programs \
    || true
