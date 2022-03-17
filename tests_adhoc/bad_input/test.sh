#!/bin/sh

set -e

! futhark test bad_input.fut > stderr
if !(fgrep -q 'Expected input of types: i64' stderr) ||
        !(fgrep -q 'Provided input of types: i32' stderr); then
    cat stderr
    exit 1
fi
