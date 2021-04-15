#!/bin/sh

for d in * ; do
    if [ -d "$d" ]; then
        cd $d
        ./test.sh || exit 1
    fi
done
