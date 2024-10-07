#!/bin/sh

if [ $# != 1 ]; then
    echo "Usage: $0 <PROG>"
    exit 1
fi

prog=$1

bef_hash=$(futhark hash "$prog")
aft_hash=$(futhark fmt < "$prog" | futhark hash)

if [ "$bef_hash" != "$aft_hash" ]; then
    echo "Tree changed."
fi
