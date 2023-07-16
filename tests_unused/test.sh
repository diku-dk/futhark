#!/bin/sh
#
# You must be in the directory when running this script.  It does not
# try to be clever.

set -e

do_test() {
    folder=$1
    in=$(basename -s .fut "$folder/main.fut").fut
    out=$(futhark unused $in)
    actual=$(basename -s .fut "$folder/actual.out").out

    echo $in

    if ! diff -u $actual $out; then
        exit 1
    fi
}

for x in *.fut; do do_test $x || exit 1; done
