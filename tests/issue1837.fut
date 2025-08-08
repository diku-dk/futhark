-- This crashed the compiler because the size slice produced by
-- ExpandAllocations had a consumption inside of it.
-- ==
-- structure gpu-mem { SegMap 2 SegRed 1 }

entry main [n] (xs: [n]i64) =
  tabulate n
           (\_ ->
              let xs = scatter (copy xs) xs xs
              let xs = xs with [opaque 0] = opaque n
              in spread xs[0] 0 xs xs :> [n]i64)
