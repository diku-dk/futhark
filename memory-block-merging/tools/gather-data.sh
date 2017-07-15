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

timeout_secs="$2"
if ! [ "$timeout_secs" ]; then
    # No limit on runtimes.
    flags=''
else
    # Effectively ignore too large datasets.
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

# Get compilation information.
get_compilation_info > \
                     "$base/compilation_without-coalescing_without-reuse.json"
IN_PLACE_LOWERING=0 MEMORY_BLOCK_MERGING_COALESCING=1 \
                 get_compilation_info > \
                    "$base/compilation_with-coalescing_without-reuse.json"
IN_PLACE_LOWERING=0 MEMORY_BLOCK_MERGING_REUSE=1 \
                 get_compilation_info > \
                   "$base/compilation_without-coalescing_with-reuse.json"
IN_PLACE_LOWERING=0 MEMORY_BLOCK_MERGING_COALESCING=1 MEMORY_BLOCK_MERGING_REUSE=1 \
                 get_compilation_info > \
                    "$base/compilation_with-coalescing_with-reuse.json"

# Get runtime information.
futhark-bench $flags --json \
              "$base/measurements_without-coalescing_without-reuse.json" . || true

IN_PLACE_LOWERING=0 MEMORY_BLOCK_MERGING_COALESCING=1 \
                 futhark-bench $flags --json \
                 "$base/measurements_with-coalescing_without-reuse.json" . || true

IN_PLACE_LOWERING=0 MEMORY_BLOCK_MERGING_REUSE=1 \
                 futhark-bench $flags --json \
                   "$base/measurements_without-coalescing_with-reuse.json" . || true

IN_PLACE_LOWERING=0 MEMORY_BLOCK_MERGING_COALESCING=1 MEMORY_BLOCK_MERGING_REUSE=1 \
                 futhark-bench $flags --json \
                   "$base/measurements_with-coalescing_with-reuse.json" . || true
