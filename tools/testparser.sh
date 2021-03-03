#!/bin/sh
#
# This script checks that the IR of various Futhark compiler pipelines
# can be parsed back in.  It does not do any correctness verification.
#
# To make the xargs hack work, you must run it as './testparser.sh'
# (or with some other path), *not* by passing it to 'sh'.

set -e

THREADS=16
dir=testparser

mkdir -p $dir

if [ "$TESTPARSER_WORKER" ]; then
    shift
    testwith() {
        f=$1
        suffix=$2
        shift; shift
        out=$dir/$(basename $f)_$suffix
        futhark dev -w "$@" $f > $out
        futhark dev $out >/dev/null
    }
    for f in "$@"; do
        if futhark check $f 2>/dev/null; then
            testwith $f soacs -s
            testwith $f mc -s --extract-multicore
            testwith $f kernels --kernels
            testwith $f mc_mem --cpu
            if ! grep -q no_opencl $f; then
                testwith $f kernels_mem --gpu
            fi
        fi
    done
else
    export TESTPARSER_WORKER=1
    find "$@" -name \*.fut -print0 | xargs -0 -n 1 -P $THREADS $0 -rec
fi
