-- ==
-- input { [2i64, 5i64, 4i64] }
-- auto output
def main (xs) =
  map (\x ->
         let zs = iota x
         let some_res =
           map (\z ->
                  let z' = iota z
                  let (_, res, _) =
                    loop (i, s, ac) = (0, 0, z')
                    while i < x do
                      let ac' = map (+ i) ac
                      let s' = reduce (+) 0 ac'
                      let s'' = s + s'
                      in (i + 1, s'', ac')
                  in res)
               zs
         in reduce (+) 0 some_res)
      xs
