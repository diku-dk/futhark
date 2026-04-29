-- ==
-- input { [[1i64, 2i64, 0i64], [4i64, 0i64, 6i64]] }
-- auto output

def main [n] [m] (xss: [n][m]i64) =
  map (\xs ->
    loop acc = xs for _i < 3 do
      let ys = iota m
      let acc' = map2 (+) acc ys
      in acc'
  ) xss
