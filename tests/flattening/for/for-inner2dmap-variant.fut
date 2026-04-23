-- ==
-- input { [[3i64, 1i64, 4i64], [2i64, 0i64, 5i64]] [13i64, 11i64, 6i64] }
-- auto output

def main [n][m][k] (xss: [n][m]i64) (ys: [k]i64) =
  map (\xs ->
    map (\x ->
      let it = opaque (x % 4)
      let zs = iota (x + 1)
      let (acc, zs_acc) =
        loop (acc, zs_acc) = (ys, zs) for i < it do
          let acc' = map (\y -> y + x + i) acc
          let zs' = map (+ i) zs_acc
          in (acc', zs')
      in reduce (+) 0 acc + i64.sum zs_acc
    ) xs
  ) xss
