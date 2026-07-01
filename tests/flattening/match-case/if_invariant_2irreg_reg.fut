-- ==
-- input { [5i64, 10i64, 15i64] [1i64, 4i64, 3i64] 5i64}
-- auto output
-- input { [5i64, 10i64, 15i64] [1i64, 4i64, 3i64] 1i64}
-- auto output
def main [n] (xs: [n]i64) (ys: [n]i64) (b: i64) =
  map2 (\x y ->
          let (if_res, if_res2,if_res3) =
            opaque (if b == 5
                    then let zs = iota x
                         let zs' = iota y
                         let lit0 = [x,4,5,x]
                         let lit0' = map (+ x) lit0
                         in (map (* x) zs, map (* y) zs', lit0')
                    else let zs = iota x
                         let zs' = iota y
                         let lit1 = [y,4,5,y,100]
                         let lit1' = map (* y) lit1
                         in (map (+ x) zs, map (+ y) zs', lit1'))
          in i64.sum (if_res ++ if_res2 ++ if_res3))
       xs
       ys
