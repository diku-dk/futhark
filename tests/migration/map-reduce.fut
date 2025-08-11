-- The array read associated with a reduce can be eliminated, and reads can be
-- delayed into map kernels.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/Apply 1
--   /Index 0
--   /SegRed 1
--   /SegMap 1
-- }

entry vector_norm [n] (A: [n]f32) : [n]f32 =
  let pow2 = map (\x -> x * x) A
  let len = reduce (+) 0 pow2 |> f32.sqrt
  in map (/ len) A
