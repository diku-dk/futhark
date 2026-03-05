-- ==
-- input { [3i64, 7i64, 100i64, 1i64, 1000i64] }
-- auto output
def main (xs) =
  map (\x ->
         let z' = iota x
         let (_, res, _) =
           loop (i, s, ac) = (0, 0, z')
           while i < x do
             let ac' = filter (\t -> t < i) ac
             let s' = reduce (+) 0 ac'
             let s'' = s + s'
             in (i + 1, s'', ac')
         in res)
      xs
