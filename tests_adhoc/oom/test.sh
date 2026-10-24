#!/bin/sh
#
# Test that running out of host memory is reported as an error, rather than
# crashing.

set -e

if [ "$(uname)" != Linux ]; then
    echo "Skipping on non-Linux system."
    exit 0
fi

LIMIT=1048576 # KiB of virtual memory.
N=268435456 # 2GiB worth of i64s.

futhark c test.fut -o oom
if (ulimit -v $LIMIT; echo ${N}i64 | ./oom) > /dev/null 2> stderr; then
    echo "Program unexpectedly succeeded."
    exit 1
fi
if ! grep -q 'Failed to allocate' stderr; then
    cat stderr
    exit 1
fi

futhark c --library test.fut -o liboom
gcc main.c liboom.c -o main -lm
(ulimit -v $LIMIT; ./main)
