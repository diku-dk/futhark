-- ==
-- compiled random input { [20000]f32 } error: Index \[-1\]

let main [n] (xs: [n]f32) =
  reduce_by_index (replicate 3 0) (+) 0
                  (map (%3) (iota n))
                  (map (\i -> xs[if i == 1000 then -1 else i]) (iota n))
