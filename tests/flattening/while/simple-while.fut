-- ==
-- input { [3i64, 7i64, 100i64, 1i64, 1000i64] }
-- auto output
def main (xs) =
  map (\x ->
        let zs = iota x
        let some_res = map (\z ->
          let z' = iota x in
          let tes = map (\y -> y+ z) z' in
          let (_,res,_) = loop (i,s,ac) = (0,0, tes) while i < x  do
            let ac' = filter (\t -> t < i) ac in
            let s' = reduce (+) 0 ac' in
            let s'' = s + s' in
            (i + 1, s'', ac')
          in res
        ) 
        zs
      in reduce (+) 0 some_res + x
  )xs
