# Memory block merging

Ongoing work.


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
    block merging.  This will always be run after the coalescing part if
    that is also enabled.


## Tests

The tests in `coalescing` expect to be run with only
`MEMORY_BLOCK_MERGING_COALESCING=1`.  Run `./test.sh` in the directory
to do so.

The tests in `reuse` (new name for "register allocation") expect to be
run with only `MEMORY_BLOCK_MERGING_REUSE=1`.  Run `./test.sh` in the
directory to do so.

The tests in `mix` (new name for "register allocation") expect to be run
with both `MEMORY_BLOCK_MERGING_COALESCING=1` and
`MEMORY_BLOCK_MERGING_REUSE=1`.  Run `./test.sh` in the directory to do
so.
