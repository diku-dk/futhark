-- Reads can be delayed and reduced inside loops.
-- ==
-- structure gpu {
--   /DoLoop/GPUBody 1
--   /DoLoop/GPUBody/Apply 1
--   /DoLoop/Index 1
-- }

def main [n] [m] (A: *[n][m]f32) : *[n][m]f32 =
  loop A = A for i < n do
    let B = A[i, :]
    let B' = map (\x -> x*x) B
    let len = reduce (+) 0 B' |> f32.sqrt
    let B' = map (/len) B'
    let A' = A with [i, :] = B'
     in A'
