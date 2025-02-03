-- ==
-- structure gpu-mem { SegMap 5 SegRed 2 }

entry main [n] (xs: [n]i64) =
  tabulate n
  (\_ ->
     let xs = loop xs = copy xs for i < 3
        do scatter (copy xs) (scan (+) 0 xs) xs
     let xs = xs with [opaque 0] = n
     in spread xs[0] 0 xs xs :> [n]i64)
