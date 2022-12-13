-- ==
-- random input { [20000]f32 } error: Index \[-1\]

def main [n] (xs: [n]f32) =
  spread 5 3 (iota n) (map (\i -> xs[if i == 1000 then -1 else i]) (iota n))
