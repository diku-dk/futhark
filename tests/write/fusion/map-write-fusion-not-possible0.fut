-- Test that map-write fusion is *not* applied when not all of the map outputs
-- are used in the write.
-- ==
-- structure { Map 1 Write 1 }

fun main(indexes: [k]i32,
       values: [k]i32,
       array: *[n]i32): ([n]i32, [n]i32) =
  let (indexes', baggage) = unzip(map (fn (i: i32, v: i32): (i32, i32) =>
                                         (i + 1, v + 1)) (zip indexes values))
  let array' = write indexes' values array
  in (array', baggage)
