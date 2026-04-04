-- ==
-- input { [1i64,2i64,3i64] }
-- auto output
entry main (xs: []i64) =
  map (\x ->
         let y = opaque (replicate 50 10 with [x] = x)
         let d = opaque (replicate 50 10 with [x] = x + 3)
         let t = opaque (replicate 50 x with [x] = 10)
         let o = iota 50
         in opaque ([y, d, t, o]))
      xs
