-- ==
-- input { [[4i64, 5i64, 6i64], [7i64, 8i64, 9i64]] [1i64, 2i64, 3i64]  }
-- auto output
def main xss ys =
  map (\xs ->
         let res =
           map (\x ->
                  let zs' = iota x
                  let r = i64.sum zs'
                  in map (+ r) ys)
               xs
         in res)
      xss
