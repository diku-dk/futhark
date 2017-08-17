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

base="$(readlink -f "$result_dir")"

cd "$(dirname "$0")/../../"
stack install

# Assumes your futhark-benchmarks directory is next to your futhark directory.
cd "../futhark-benchmarks/"

mkdir "$base"
base="$base/runs"
mkdir "$base"

# Get runtime measurements.
futhark-bench $flags --json \
              "$base/measurements_without-coalescing_without-reuse.json" . \
    || true

export IN_PLACE_LOWERING=0
export MEMORY_BLOCK_MERGING_COALESCING=1
export MEMORY_BLOCK_MERGING_REUSE=1
futhark-bench $flags --json \
              "$base/measurements_with-coalescing_with-reuse.json" . \
    || true
