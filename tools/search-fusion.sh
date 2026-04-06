#!/bin/sh
#
# This script runs the fusion algorithm pass on an input program using the gas
# parameter. It finds the maximal amount gas before fusion fails, if fusion does
# not fail it will return the maximum i64 integer. This helps in determining at
# which point fusion fails when debugging the fusion pass.

[ "$#" -ne 1 ] && echo "Usage: $0 <path>" && exit 1
[ ! -f "$1" ] && echo "File not found: $1" && exit 1

test_n() { cabal run -- futhark dev --inline-cons --inline-cons -e --fuse=$1 "$2" &>/dev/null; }

N=1
! test_n $N "$1" && exit 1

I64_MAX=9223372036854775807
while test_n $N "$1"; do
    LAST=$N
    [ $N -gt $((I64_MAX / 2)) ] && echo $I64_MAX && exit 0
    N=$((N * 2))
done

LOW=$LAST
HIGH=$N

while [ $((HIGH - LOW)) -gt 1 ]; do
    MID=$((LOW + (HIGH - LOW) / 2))
    test_n $MID "$1" && LOW=$MID || HIGH=$MID
done

echo $LOW
