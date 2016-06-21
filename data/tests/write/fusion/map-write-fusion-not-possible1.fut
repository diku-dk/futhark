-- Test that map-write fusion is *not* applied when one of the I/O arrays (which
-- are not part of the fusable indexes and values arrays) are used in the map
-- *and* in the write.  If this was fused into a single write, the I/O array
-- would ultimately be written and read in the same kernel.
-- ==
-- structure { Map 1 Write 1 }

fun [n]i32
  main([k]i32 indexes,
       [k]i32 values,
       *[n]i32 array) =
  let indexes' = map(fn i32 (i32 i) => unsafe array[i], indexes)
  let array' = write(indexes', values, array)
  in array'
