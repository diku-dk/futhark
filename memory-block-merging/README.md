# Memory block merging

Ongoing work.  This directory really should be removed at some point,
and its contents be merged into the proper general directories.


## Debug environment variables

This is an overview of the available environment variables.  Most of
this is hopefully temporary.

  + Set `FUTHARK_DEBUG=1`: Print a lot of messy, unexplained debug
    information.
  + Set `FUTHARK_DEBUG_JSON=1`: Print whatever calculations are deemed
    important in machine-readable JSON lines, i.e. every line is valid
    JSON, but the output as a whole probably is not.
  + Set `IN_PLACE_LOWERING=0`: Disable in-place lowering.
  + Set `MEMORY_BLOCK_MERGING_COALESCING=1`: Enable the coalescing part
    of memory block merging.
  + Set `MEMORY_BLOCK_MERGING_REUSE=1`: Enable the reuse part of memory
    block merging (new name for "register allocation").  This will
    always be run after the coalescing part if that is also enabled.


## Testing

The tests in `coalescing` expect to be run with only
`MEMORY_BLOCK_MERGING_COALESCING=1`.  Run `./test.sh` in the directory
to do so.

The tests in `reuse` expect to be run with only
`MEMORY_BLOCK_MERGING_REUSE=1`.  Run `./test.sh` in the directory to do
so.

The tests in `mix` expect to be run with both
`MEMORY_BLOCK_MERGING_COALESCING=1` and `MEMORY_BLOCK_MERGING_REUSE=1`.
Run `./test.sh` in the directory to do so.


## Overview printing

You can get a nice indentation-supported overview of a program by
running futhark with `MEMORY_BLOCK_MERGING_OVERVIEW_PRINT=1`.  For
example, running

```
MEMORY_BLOCK_MERGING_REUSE=1 \
MEMORY_BLOCK_MERGING_OVERVIEW_PRINT=1 \
futhark --gpu memory-block-merging/tests/reuse/chain/chain0.fut
```

will give you this simplified overview of the program:

```
main =
    kernel map
     | res_2263 | memory block: mem_2333
     | res_2263 | available for reuse:
     | res_2263 | base interferences: ns_mem_2330
    kernel map
     | res_2271 | memory block: mem_2336
     | res_2271 | available for reuse: (mem_2333, [mem_2333])
     | res_2271 | base interferences:
    replicate([size_2257], res_2278)
     | res_2289 | memory block: mem_2339
     | res_2289 | available for reuse: (mem_2333, [mem_2333, mem_2336])
     | res_2289 | base interferences:
```

This mode will not show you *every* bit of information that you might
want; its purpose is to ignore details and show you the big picture.  In
this case it shows that the main function consists of two map kernels
and a replicate, and that everything can be set to use the memory block
of the first kernel.


## Benchmarking

For benchmarking, check out the niels-memory-block-merging-benchmarking
branch.
