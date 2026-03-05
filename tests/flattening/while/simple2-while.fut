-- ==
-- input { [3i64, 7i64, 1i64, 10i64] }
-- auto output
def main [n] (xs: [n]i64) =
  map (\x ->
         let zs = iota x
         let some_res =
           map (\z ->
                  let res = loop acc = z while acc < 10 do acc + 2
                  -- let res = z * 100
                  in res)
               zs
         in reduce (+) 0 some_res + x)
      xs
