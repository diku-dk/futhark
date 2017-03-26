-- Test that map-write fusion is *not* applied when one of the I/O arrays (which
-- are not part of the fusable indexes and values arrays) are used in the map
-- *and* in the write.  If this was fused into a single write, the I/O array
-- would ultimately be written and read in the same kernel.
-- ==
-- structure { Map 1 Write 1 }

let main(indexes: [k]i32,
       values: [k]i32,
       array: *[n]i32): [n]i32 =
  let indexes' = map (\(i: i32): i32 -> unsafe array[i]) indexes
  let array' = write indexes' values array
  in array'
