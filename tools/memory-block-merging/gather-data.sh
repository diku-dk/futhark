#!/bin/sh
#
# Gather data from both the compilation phase and benchmarking.
#
# The sole argument should be a directory name.  This script will then create
# that directory and store gathered data in multiple JSON files in a
# subdirectory "runs".
#
# When this script has completed, you can run './merge-data.py' to gather all
# the data into a single JSON file.

set -e # Exit on first error.

result_dir="$1"
if ! [ "$result_dir" ]; then
    echo 'error: specify output directory as first argument' > /dev/stderr
    exit 1
fi

timeout_secs="$FUTHARK_BENCH_MEMORY_BLOCK_MERGING_TIMEOUT"
if [ "$timeout_secs" = '0' ]; then
    # No limit on runtime.
    flags=''
else
    if ! [ "$timeout_secs" ]; then
        timeout_secs=60
    fi
    # Ignore too large datasets.
    flags="--timeout $timeout_secs"
fi

base="$(readlink -f "$result_dir")"

# Assumes your futhark-benchmarks directory is next to your futhark directory.
cd "$(dirname "$0")/../../../futhark-benchmarks/"

mkdir "$base"
base="$base/runs"
mkdir "$base"

# First get compilation information for every program, and put it in its own
# JSON file.
get_compilation_info() {
    temp_storage="$(mktemp)"
    {
        export MEMORY_BLOCK_MERGING=1
        export FUTHARK_DEBUG_JSON=1
        echo '{'
        find -name '*.fut' | while read file; do
            echo "Getting compilation information for $file." > /dev/stderr
            futhark --cpu $file 2> /dev/null > "$temp_storage" && \
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

get_compilation_info > "$base/compilation_in-place-lowering_memory-block-merging.json"
IN_PLACE_LOWERING=0 get_compilation_info > "$base/compilation_no-in-place-lowering_memory-block-merging.json"
# It doesn't make sense to run get_compilation_info with memory block merging
# disabled, since that will just give us less information.

# Then run different versions of the compiler on the datasets.
futhark-bench $flags --json \
              "$base/measurements_in-place-lowering_no-memory-block-merging.json" . || true

IN_PLACE_LOWERING=0 futhark-bench $flags --json \
                 "$base/measurements_no-in-place-lowering_no-memory-block-merging.json" . || true

MEMORY_BLOCK_MERGING=1 futhark-bench $flags --json \
                    "$base/measurements_in-place-lowering_memory-block-merging.json" . || true

IN_PLACE_LOWERING=0 MEMORY_BLOCK_MERGING=1 futhark-bench $flags --json \
                 "$base/measurements_no-in-place-lowering_memory-block-merging.json" . || true
