-- ==
-- input { [[4i64, 5i64, 6i64], [7i64, 8i64, 9i64]] [1i64, 2i64, 3i64]  }
-- auto output
-- structure gpu { SegScan 3 SegRed 0 }

def main xss ys =
  #[incremental_flattening(only_inner)]
  map (\xs ->
         let res =
           map (\x ->
                  let zs' = iota x
                  let r = i64.sum zs'
                  in map (+ r) ys)
               xs
         in res)
      xss
