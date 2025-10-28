-- ==
-- random input { [20000]f32 } error: Index \[-1\]

def main [n] (xs: [n]f32) =
  hist (+)
       0
       3
       (map (% 3) (iota n))
       (map (\i -> xs[if i == 1000 then -1 else i]) (iota n))
