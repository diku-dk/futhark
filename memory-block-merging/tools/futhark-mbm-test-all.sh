#!/bin/sh
#
# Perform all possible memory block merging tests.

section() {
    echo "# $*"
}

with_opencl() {
    prog="$1"
    shift
    time "$prog" futhark-opencl "$@"
}

base="$(dirname "$0")/../../../futhark/memory-block-merging/tests/" # Makes assumption.

section Memory block merging tests, futhark-c.
time "$base"/test_all.sh

section Memory block merging tests, futhark-opencl.
with_opencl "$base"/test_all.sh


base="$(dirname "$0")"

section Futhark tests, futhark-c.
time "$base"/futhark-mbm-failures

section Futhark tests, futhark-opencl.
with_opencl "$base"/futhark-mbm-failures

section Futhark benchmarks, futhark-c.
time "$base"/futhark-mbm-failures-benchmarks

section Futhark benchmarks, futhark-opencl.
with_opencl "$base"/futhark-mbm-failures-benchmarks
