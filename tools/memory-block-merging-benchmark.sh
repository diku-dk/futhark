#!/bin/sh

base="$(readlink -f "$PWD")"

# Assumes your futhark-benchmarks directory is next to your futhark directory.
cd "$(dirname "$0")/../../futhark-benchmarks/"

# Ignore too large datasets.
flags='--timeout 60'

futhark-bench $flags --json "$base/without.json" .

MEMORY_BLOCK_MERGING=1 futhark-bench $flags --json "$base/with.json" .
