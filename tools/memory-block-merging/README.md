# Memory block merging benchmarking

Default limits: To make the full benchmarking finish reasonably fast,
runs are set to time out after 60 seconds by default.  Change this by
setting the environment variable
`FUTHARK_BENCH_MEMORY_BLOCK_MERGING_TIMEOUT` to a number of seconds, or
0 if there should be no limit.  Runs below 1 millisecond are always
ignored.

First run `./gather-data.sh OUT_DIRECTORY` to run all the
futhark-benchmark programs multiple times with different settings.

Then run `./merge-data.py OUT_DIRECTORY` to gather all results in one
cleaned-up JSON file named `full.json` (is quick).

At this point you can run `./summarize.py OUT_DIRECTORY` to get a
pretty-printed summary of the results in the full JSON file, and you can
run `./plot.py OUT_DIRECTORY` to get several plots for every benchmark.

See the individual files for more comments on usage and effects.


# Structure in output `full.json` from `./merge-data.py`:

```
top-level: { benchmark_name: benchmark_info }
           for every benchmark_name of the benchmarks

benchmark_info: { 'compilation': compilation_info
                , 'datasets': datasets
                }

compilation_info: { 'without-in-place-lowering-without-memory-block-merging': compilation_info_base
                  ,    'without-in-place-lowering-with-memory-block-merging': compilation_info_base
                  ,    'with-in-place-lowering-without-memory-block-merging': compilation_info_base
                  ,       'with-in-place-lowering-with-memory-block-merging': compilation_info_base
                  }
                  where compilation_info_base is just whatever JSON the
                  compiler spouts

datasets: { dataset_name: dataset_info }
          for every dataset_name of the benchmark

dataset_info: { 'without-in-place-lowering-without-memory-block-merging': dataset_info_base
              ,    'without-in-place-lowering-with-memory-block-merging': dataset_info_base
              ,    'with-in-place-lowering-without-memory-block-merging': dataset_info_base
              ,       'with-in-place-lowering-with-memory-block-merging': dataset_info_base
              }

dataset_info_base: { 'average_runtime': microseconds
                   , 'total_cumulative_allocations': bytes
                   , 'total_cumulative_frees': bytes
                   , 'peak_memory_usages': peak_memory_usages
                   }

peak_memory_usages: { space_name: bytes }
                    for every space_name in the available memory spaces
```
