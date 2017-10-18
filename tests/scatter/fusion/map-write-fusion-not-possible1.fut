-- Test that map-scatter fusion is *not* applied when one of the I/O arrays (which
-- are not part of the fusable indexes and values arrays) are used in the map
-- *and* in the scatter.  If this was fused into a single scatter, the I/O array
-- would ultimately be written and read in the same kernel.
-- ==
-- structure { Map 1 Scatter 1 }

let main [k][n] (indexes: [k]i32,
                 values: [k]i32,
                 array: *[n]i32): [n]i32 =
  let indexes' = map (\(i: i32): i32 -> unsafe array[i]) indexes
  let array' = scatter array indexes' values
  in array'
