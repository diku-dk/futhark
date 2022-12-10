-- ==
-- structure gpu { SegRed 1 }

entry main [n] [d] (as: [n][d]f32) : [3]f32 =
  let bs = replicate n 0
  let f i = f32.sum as[i] + 1 + bs[i]
  in tabulate 3 f
