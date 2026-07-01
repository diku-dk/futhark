-- ==
-- input { [[1i64, 2i64, 3i64], [4i64, 5i64, 6i64]] [13i64, 11i64, 6i64] }
-- auto output

def main [n][m][k] (xss: [n][m]i64) (ys: [k]i64)  =
  map (\xs->
         let xs' = map (*2) xs in
         map (\x -> 
              loop  acc = ys for i < 10 do 
                let ys' = map (+ i ) acc
                in map (+ x) ys'
         ) xs'
  )  xss
