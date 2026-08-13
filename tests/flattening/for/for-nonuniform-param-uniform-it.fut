-- ==
-- input { [10i64,7i64,0i64,10i64] 8i64 }
-- auto output
-- structure gpu { /Loop/ForLoop 1 }

def main [n] (xs: [n]i64) (i: i64) =
  #[flattening(only_inner)]
  map (\x ->
         let ys = iota x
         let (acc_res, _) =
           loop (acc, j) = (ys, 1)
           for i < i do
             let acc' = map (\y -> y * j) acc
             let j' = j + i
             in (acc', j')
         in reduce (+) 0 (acc_res))
      xs
