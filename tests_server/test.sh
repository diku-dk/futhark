#!/bin/sh

BACKEND=c

for d in *; do
    if [ -d "$d" ]; then
        echo "# $d"
        futhark $BACKEND "$d/test.fut" --server
        if ! (cd "$d" && ./test < input.txt > output.txt.actual); then
            echo Server failed
            exit 1
        fi
        if ! diff -u "$d/output.txt.expected" "$d/output.txt.actual"; then
           echo Invalid server output
           exit 1
        fi
        if ! futhark datacmp "$d/output.data.expected" "$d/output.data.actual"; then
           echo Invalid server values
           exit 1
        fi
    fi
done
