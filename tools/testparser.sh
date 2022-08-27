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
        out=$dir/${f}_${suffix}
        mkdir -p "$(dirname "$out")"
        if ! ( futhark dev -w "$@" "$f" > "$out" && futhark dev "$out" >/dev/null); then
            echo "^- $f $*"
            exit 1
        fi
    }
    for f in "$@"; do
        if futhark check "$f" 2>/dev/null && ! (grep -F 'tags { disable }' -q "$f") ; then
            testwith "$f" soacs -s
            testwith "$f" mc -s --extract-multicore
            testwith "$f" gpu --gpu
            testwith "$f" mc_mem --mc-mem
            if ! grep -q no_opencl "$f"; then
                testwith "$f" gpu_mem --gpu-mem
            fi
        fi
    done
else
    export TESTPARSER_WORKER=1
    find "$@" -name \*.fut -print0 | xargs -0 -n 1 -P $THREADS "$0" -rec
fi
