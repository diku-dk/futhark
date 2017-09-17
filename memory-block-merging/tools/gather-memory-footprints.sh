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

"$(dirname "$0")/gather-data-coarse.sh" "${result_dir_base}-cpu" futhark-c '' 1

"$(dirname "$0")/gather-data-coarse.sh" "${result_dir_base}-gpu" futhark-opencl '' 1
