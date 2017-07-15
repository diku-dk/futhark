#!/bin/sh

set -e # Exit on first error.

result_dir="$1"
if ! [ "$result_dir" ]; then
    echo 'error: specify output directory as first argument' > /dev/stderr
    exit 1
fi

search_dir="$2"
if ! [ "$search_dir" ]; then
    # Assumes your futhark-benchmarks directory is next to your futhark directory.
    search_dir="$(dirname "$0")/../../../futhark-benchmarks/"
fi

base="$(readlink -f "$result_dir")"
cd "$search_dir"

without="$(mktemp)"
with="$(mktemp)"

find -name '*.fut' | while read file; do
    IN_PLACE_LOWERING=0 futhark --cpu "$file" > "$without" || continue
    futhark --cpu "$file" > "$with" || continue
    if ! cmp -s "$without" "$with"; then
        mkdir -p "$base/$(dirname "$name")"
        diff "$without" "$with" > "$base/$(dirname "$name")"
    fi
done
