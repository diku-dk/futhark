#!/bin/sh

set -e

do_test() {
    prog=$1
    in=$(basename -s .fut $prog).in
    out=$(basename -s .fut $prog).out
    actual=$(basename -s .fut $prog).actual

    echo $prog

    futhark repl $prog < $in > $actual
    if ! diff -u $actual $out; then
        exit 1
    fi
}

for x in *.fut; do do_test $x || exit 1; done
