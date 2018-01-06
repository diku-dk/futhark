#!/bin/sh
#
# Focus on getting good runtime data (will also get memory footprint).  Gathers
# both CPU and GPU data.

set -ex # Exit on first error, be verbose.

result_dir_base="$1"
if ! [ "$result_dir_base" ]; then
    echo 'error: specify output directory base as first argument' > /dev/stderr
    exit 1
fi

benchmark_programs="$2"

"$(dirname "$0")/gather-data.sh" "${result_dir_base}-gpu" futhark-opencl '' '' "$benchmark_programs"

# This will take a much longer time.
"$(dirname "$0")/gather-data.sh" "${result_dir_base}-cpu" futhark-c '' '' "$benchmark_programs"
