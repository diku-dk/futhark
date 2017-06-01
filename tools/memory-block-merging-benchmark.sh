#!/bin/sh

set -e # Exit on first error.

# Assumes your futhark-benchmarks directory is next to your futhark directory.
cd "$(dirname "$0")/../../futhark-benchmarks/"

# Ignore too large datasets.
flags='--timeout 60'

futhark-bench $flags --json without.json .

MEMORY_BLOCK_MERGING=1 futhark-bench $flags --json with.json .
