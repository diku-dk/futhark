-- ==
-- input { [5i64, 10i64, 15i64] [10i64, 8i64, 6i64] 5i64 }
-- auto output
-- input { [5i64, 10i64, 15i64] [10i64, 8i64, 6i64] 0i64 }
-- auto output
def main [n] (xs: [n]i64) (ys: [n]i64) (b: i64) =
  map2 (\x y ->
          let (if_res, if_res2) =
            opaque (if b == 5
                    then let zs = iota x
                         let zs' = iota y
                         in (map (+ x) zs, map (* y) zs')
                    else let zs = iota x
                         let zs' = iota y
                         in (map (+ x) zs, map (+ y) zs'))
          in if_res[1] + if_res2[1])
       xs
       ys
