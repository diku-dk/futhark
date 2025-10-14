-- Test that map-write fusion is *not* applied when not all of the map outputs
-- are used in the write.
-- ==
-- structure { Screma 1 }

def main [k] [n]
         ( indexes: [k]i64
         , values: [k]i32
         , array: *[n]i32
         ) : ([n]i32, [k]i32) =
  let (indexes', baggage) =
    unzip (map (\(i, v) ->
                  (i + 1, v + 1))
               (zip indexes values))
  let array' = scatter array indexes' values
  in (array', baggage)
