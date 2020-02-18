-- ==
-- compiled random input { [20000]f32 } error: Index \[-1\]

let main [n] (xs: [n]f32) =
  f32.sum (map (\i -> xs[if i == 1000 then -1 else i]) (iota n))
