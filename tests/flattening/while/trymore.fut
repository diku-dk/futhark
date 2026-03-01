-- ==
-- input { [2i64 , 10, 4] }
-- auto output
def main (xs) =
  map (\x ->
         let zs = iota x
         let some_res =
           map (\z ->
                  let z' = iota z
                  let (_, res) =
                    loop (i, ac) = (0, z')
                    while i < x do
                      let ac' = map (\t -> t + i) ac
                      in (i + 1, ac')
                  in reduce (+) 0 res)
               zs
         in reduce (+) 0 some_res )
      xs
