-- Test that map-scatter fusion is *not* applied when one of the I/O arrays (which
-- are not part of the fusable indexes and values arrays) are used in the map
-- *and* in the scatter.  If this was fused into a single scatter, the I/O array
-- would ultimately be written and read in the same kernel.
-- ==
-- structure { Screma 2 }

def main [k] [n]
         ( indexes: [k]i64
         , values: [k]i64
         , array: *[n]i64
         ) : [n]i64 =
  let indexes' = map (\i -> array[i]) indexes
  let array' = scatter array indexes' values
  in array'
