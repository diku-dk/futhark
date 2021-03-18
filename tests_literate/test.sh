#!/bin/sh

for x in *.fut; do
    md=${x/.fut/.md}
    echo
    echo "$x:"
    futhark literate $x
    if ! diff -u expected/$md $md; then
        exit 1
    fi
done
