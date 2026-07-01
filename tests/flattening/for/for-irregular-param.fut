-- ==
-- input { [10i64,7i64,3i64] [10i64,7i64,3i64]}
-- auto output
def main [n] (xs: [n]i64) (is: [n]i64) =
  map2 (\xs it ->
          let ys = iota xs
          let (acc_res, _) =
            loop (acc, j) = (ys, 1)
            for i < it do
              let acc' = map (\y -> y * j) acc
              let j' = j + i
              in (acc', j')
          in reduce (+) 0 (acc_res))
       xs
       is