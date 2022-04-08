-- The array read associated with a reduce can be eliminated.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/Apply 1
--   /Index 0
-- }

def main [n] (A: [n]f32): [n]f32 =
  let pow2 = map (\x -> x*x) A
  let len = reduce (+) 0 pow2 |> f32.sqrt
  in  map (/len) A