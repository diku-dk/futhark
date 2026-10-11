#!/bin/sh

for x in *.fut; do
    md=$(basename -s .fut $x).md
    echo
    echo "$x ($(futhark hash $x)):"
    rm -rf $(basename -s .fut $x)-img
    futhark literate $x
    if ! diff -u expected/$md $md; then
        exit 1
    fi
done
