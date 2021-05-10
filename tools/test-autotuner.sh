#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

for bench in rodinia/lud/lud misc/bfast/bfast finpar/LocVolCalib accelerate/nbody/nbody
do
    futhark autotune --backend=opencl --tuning=.tuning.test futhark-benchmarks/$bench.fut
    diff -u futhark-benchmarks/$bench.fut.tuning futhark-benchmarks/$bench.fut.tuning.test
    rm -f futhark-benchmarks/$bench.fut.tuning.test
done
