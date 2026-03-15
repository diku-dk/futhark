-- ==
-- input { [[1i64, 2i64, 3i64], [4i64, 5i64, 6i64]] }
-- auto output

def main [n] [m] (xss: [n][m]i64) =
  map (\xs ->
    loop acc = xs for i < 3 do
      let s = reduce (+) 0 acc
      let ys = replicate m (s + i)
      let acc' = map2 (+) acc ys
      in acc'
  ) xss
