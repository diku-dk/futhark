-- ==
-- input { [[1i64, 2i64, 3i64], [4i64, 5i64, 6i64]] [13i64, 11i64, 6i64] }
-- auto output

def main [n][m][k] (xss: [n][m]i64) (ys: [k]i64)  =
  map (\xs->
         map (\x -> 
              loop  acc = ys for i < 10 do 
                let ys' = map (+ i ) acc
                let zs = iota x
                let zs' = map (*i) zs
                let sum_zs = i64.sum zs'
                in map (+ sum_zs) ys'
         ) xs
  )  xss
