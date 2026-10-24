#!/bin/sh

set -e

! futhark test bad_input.fut > stderr
if !(fgrep -q 'Expected input of types: i64' stderr) ||
        !(fgrep -q 'Provided input of types: i32' stderr); then
    cat stderr
    exit 1
fi

# A binary [2^32][2^32]i32 array whose element count overflows, with no
# elements.
futhark c bad_input.fut
if printf 'b\002\002 i32\000\000\000\000\001\000\000\000\000\000\000\000\001\000\000\000' \
        | ./bad_input -e mat 2> stderr; then
    echo "Overflowing shape was accepted."
    exit 1
fi
if ! fgrep -q 'has too many elements' stderr; then
    cat stderr
    exit 1
fi
