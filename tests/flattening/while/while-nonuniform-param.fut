-- ==
-- input { [3i64, 7i64, 10i64, 1i64, 20i64] }
-- auto output
-- structure gpu { /Loop 1 }

def main (xs) =
  #[incremental_flattening(only_inner)]
  map (\x ->
         let zs = iota x
         let (_, b) =
           loop (i, zs) = (0, zs)
           while i < x do
             let tes = map (\z -> z * i + x) zs
             in (i + 1, tes)
         in reduce (+) 0 b + x)
      xs
