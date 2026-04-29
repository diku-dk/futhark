-- ==
-- input { [5i64, 10i64, 15i64] 5i64 }
-- auto output
-- input { [5i64, 10i64, 15i64, 20i64, 25i64] 50i64 }
-- auto output
def main [n] (xs: [n]i64) (b: i64) =
  map (\x ->
         let if_res =
           opaque (if b == 5
                   then let zs = iota x
                        let zs = map (\e -> e + 5 + x) zs
                        in map (+ x) zs
                   else let zs = iota x
                        let zs = map (\e -> e * 5 + x) zs
                        in map (+ x) zs)
         in if_res[0])
      xs
