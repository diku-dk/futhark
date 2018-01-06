#!/bin/sh
#
# Focus only on getting memory footprint usage.  Makes for a faster run.
# Gathers both CPU and GPU data.

set -ex # Exit on first error, be verbose.

result_dir_base="$1"
if ! [ "$result_dir_base" ]; then
    echo 'error: specify output directory base as first argument' > /dev/stderr
    exit 1
fi

benchmark_programs="$2"

"$(dirname "$0")/gather-data.sh" "${result_dir_base}-gpu" futhark-opencl '' 1 "$benchmark_programs"

# This will take a longer time.
"$(dirname "$0")/gather-data.sh" "${result_dir_base}-cpu" futhark-c '' 1 "$benchmark_programs"
