#!/bin/sh

set -e

with_backend() {
    echo
    echo "Trying backend $1"
    $2 futhark bench --backend=$1 --profile --json prog.json prog.fut
    rm -rf prog.prof
    futhark profile prog.json
}

with_backend c

with_backend multicore

with_backend opencl oclgrind
