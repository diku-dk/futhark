-- Important thing is that the loop is not interchanged.
-- ==
-- input { [3i64, 7i64, 1i64, 10i64] }
-- auto output
-- structure gpu { /SegScan/Loop 1 Loop 1 }

def main [n] (xs: [n]i64) =
  #[incremental_flattening(only_inner)]
  map (\x ->
         let zs = iota x
         let some_res =
           map (\z ->
                  let res = loop acc = z while acc < 10 do acc + 2
                  in res)
               zs
         in reduce (+) 0 some_res + x)
      xs
