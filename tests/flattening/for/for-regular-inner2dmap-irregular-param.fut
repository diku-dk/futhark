-- ==
-- input { [[1i64, 2i64, 3i64], [4i64, 5i64, 6i64]] [13i64, 11i64, 6i64] }
-- auto output

def main [n][m][k] (xss: [n][m]i64) (ys: [k]i64)  =
  map (\xs->
         map (\x -> 
              let zs = iota x
              let (acc, _) =
                loop (acc, zs_acc) = (ys, zs) for i < 10 do
                  let ys' = map (+ i) acc
                  let zs' = map (+ i) zs_acc
                  let sum_zs = i64.sum zs'
                  let acc' = map (+ sum_zs) ys'
                  in (acc', zs')
              in acc
         ) xs
  )  xss
