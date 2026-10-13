-- ==
-- input { [5i64, 10i64, 15i64] [10i64, 14i64, 8i64] 5i64 }
-- auto output
-- input { [5i64, 10i64, 15i64, 3i64] [10i64, 14i64, 8i64, 100i64] 1i64 }
-- auto output
def main [n] (xs: [n]i64) (ys: [n]i64) (b: i64) =
  map2 (\x y ->
          let (if_res) =
            opaque (if b == 5
                    then let zs = iota x
                         in (map (+ x) zs)
                    else let lit1 = [y, 4, 5, y, 100]
                         let lit1' = map (* y) lit1
                         in (lit1'))
          in if_res[4])
       xs
       ys
