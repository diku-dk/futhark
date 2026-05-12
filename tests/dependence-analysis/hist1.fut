-- ==
-- structure { Screma/Hist 1 }

-- The two reduce_by_index produce two separate histogram operations.
def main [m] [n] (A: [m]([n]i32, [n]i32)) =
  let r =
    loop A for _i < n do
      map (\(a, b) ->
             ( reduce_by_index (replicate n 0) (+) 0 (map i64.i32 a) a
             , reduce_by_index (replicate n 0) (+) 0 (map i64.i32 b) b
             ))
          A
  in map (.0) r
