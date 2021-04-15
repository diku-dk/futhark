#!/bin/sh

# Autotune with CUDA
futhark autotune --backend=cuda simple.fut || exit 1

# We expect that main.suff_intra_par_1 is less than 2000000000:
grep "main.suff_intra_par_1=2000000000" simple.fut.tuning && exit 1

# The test was successful.
exit 0
