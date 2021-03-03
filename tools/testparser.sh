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
        out=$dir/$(basename $1)_$3
        futhark dev -w $2 $1 > $out
        futhark dev $out >/dev/null
    }
    for f in "$@"; do
        if futhark check $f 2>/dev/null; then
            testwith $f -s soacs
            testwith $f --kernels kernels
        fi
    done
else
    export TESTPARSER_WORKER=1
    find "$@" -name \*.fut -print0 | xargs -0 -n 1 -P $THREADS $0 -rec
fi
