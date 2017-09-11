#!/bin/sh

set -x

with_opencl() {
    prog="$1"
    shift
    "$prog" futhark-opencl "$@"
}

base="$(dirname "$0")/../../../futhark/memory-block-merging/tests/" # Makes assumption.
"$base"/test_all.sh
with_opencl "$base"/test_all.sh

base="$(dirname "$0")"
"$base"/futhark-mbm-failures
with_opencl "$base"/futhark-mbm-failures
"$base"/futhark-mbm-failures-benchmarks
with_opencl "$base"/futhark-mbm-failures-benchmarks
