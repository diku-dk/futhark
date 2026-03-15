-- ==
-- input { [10i64,7i64,0i64,10i64] 8i64 }
-- auto output
def main [n] (xs: [n]i64) (i :i64) =
  map (\xs  ->
          let ys = iota xs
          let (acc_res, _) =
            loop (acc, j) = (ys, 1)
            for i < i do
              let acc' = map (\y -> y * j) acc
              let j' = j + i
              in (acc', j')
          in reduce (+) 0 (acc_res))
       xs