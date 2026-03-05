-- ==
-- input { [2i64,3] [2i64,1] }
-- auto output
def main (xs) (ys) =
  map (\x ->
         let zs = iota x
         let some_res =
           map (\y ->
                  let zs' = map (+ y) zs
                  let res = i64.sum zs'
                  in res)
               ys
         in i64.sum some_res)
      xs
