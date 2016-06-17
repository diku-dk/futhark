-- Test that map-write fusion is *not* applied when not all of the map outputs
-- are used in the write.
-- ==
-- structure { Map 1 Write 1 }

fun ([n]i32, [n]i32)
  main([k]i32 indexes,
       [k]i32 values,
       *[n]i32 array) =
  let (indexes', baggage) = unzip(map(fn (i32, i32) (i32 i, i32 v) =>
                                        (i + 1, v + 1), zip(indexes, values)))
  let array' = write(indexes', values, array)
  in (array', baggage)
