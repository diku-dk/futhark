-- ==
-- structure { Screma/Hist/BinOp 1 }

-- The two reduce_by_index get fused into a single histogram operation.
def main [m] [n] (A: [m]([n]i32, [n]i32)) =
  let r =
    loop A for _i < n do
      map (\(a, b) ->
             ( reduce_by_index (replicate n 0) (+) 0 (map i64.i32 a) a
             , reduce_by_index (replicate n 0) (+) 0 (map i64.i32 a) b
             ))
          A
  in map (.0) r
