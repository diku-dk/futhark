-- ==
-- input { [0i64, 0i64, 0i64] [34i64,5i64, 9i64] }
-- auto output
-- structure gpu { /SegMap 1 /SegRed 1 }

entry main (xs: []i64) (ys: []i64) =
  #[incremental_flattening(only_inner)]
  map (\x ->
         let d = reduce (+) x ys
         in d + x)
      xs
