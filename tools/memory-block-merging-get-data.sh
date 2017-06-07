#!/bin/sh

set -e # Exit on first error.

result_dir="$1"
if ! [ "$result_dir" ]; then
    echo 'error: specify output directory as first argument' > /dev/stderr
    exit 1
fi

# Ignore too large datasets.
flags='--timeout 60'

base="$(readlink -f "$result_dir")"

# Assumes your futhark-benchmarks directory is next to your futhark directory.
cd "$(dirname "$0")/../../futhark-benchmarks/"

mkdir -p "$base"

# First get compilation information for every program, and put it in its own
# JSON file.  Requires GNU tail.
get_compilation_info() {
    temp_storage="$(mktemp)"
    {
        export MEMORY_BLOCK_MERGING=1
        export FUTHARK_DEBUG_JSON=1
        echo '{'
        find -name '*.fut' | while read file; do
            futhark --cpu $file 2> /dev/null > "$temp_storage" && \
                {
                    echo ','
                    echo '"'"$file"'":'
                    echo '['
                    cat "$temp_storage" | while read line; do
                        echo ','
                        echo "$line"
                    done | tail -n +2
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
                 "$base/measurements_no-in-place-lowering_no-memory-block-merging.json" . || true
