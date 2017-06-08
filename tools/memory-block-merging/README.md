# Memory block merging benchmarking

Default limits: To make the full benchmarking finish reasonably fast,
runs are set to time out after 60 seconds by default.  Change this by
setting the environment variable
`FUTHARK_BENCH_MEMORY_BLOCK_MERGING_TIMEOUT` to a number of seconds, or
0 if there should be no limit.  Runs below 1 millisecond are always
ignored.

First run `./gather-data.sh OUT_DIRECTORY` to run all the
futhark-benchmark programs multiple times with different settings.

Then run `./merge-data.sh OUT_DIRECTORY` to gather all results in one
cleaned-up JSON file (is quick).

At this point you can run `./summarize.py OUT_DIRECTORY` to get a
pretty-printed summary of the results in the full JSON file, and you can
run `./plot.py OUT_DIRECTORY` to get several plots for every benchmark.

See the individual files for more comments on usage and effects.
