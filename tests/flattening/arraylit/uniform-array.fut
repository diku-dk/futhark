-- The array literal is built with a concatenation outside the kernels, rather
-- than by having each thread copy 4x50 elements into its own row.  The final
-- SegMap is the 'opaque', which cannot be part of the same group as the
-- distributed array literal.
-- ==
-- input { [1i64,2i64,3i64] }
-- auto output
-- structure gpu { SegScan 0 SegMap 4 SegMap/ArrayLit 0 }

entry main (xs: []i64) =
  map (\x ->
         let y = opaque (replicate 50 10 with [x] = x)
         let d = opaque (replicate 50 10 with [x] = x + 3)
         let t = opaque (replicate 50 x with [x] = 10)
         let o = iota 50
         in opaque ([y, d, t, o]))
      xs
