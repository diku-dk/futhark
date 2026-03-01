-- ==
-- input { [2i64] }
-- auto output
def main (xs) =
  map (\x ->
         let zs = iota x
         let some_res =
           map (\z ->
                  let z' = iota x
                  let tes = map (\y -> y + z) z'
                  let (_, res, _) =
                    loop (i, s, ac) = (0, 0, tes)
                    while i < x do
                      let ac' = map (\t -> t + i) ac
                      let s' = reduce (+) 0 ac'
                      let s'' = s + s'
                      in (i + 1, s'', ac')
                  in res)
               zs
         in reduce (+) 0 some_res + x)
      xs
