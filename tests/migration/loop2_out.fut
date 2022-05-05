-- Reads can be delayed out of loops.
-- ==
-- structure gpu {
--   /DoLoop/Index 0
--   /DoLoop/GPUBody 1
--   /Index 1
-- }

def main [n] (A: [n]f32) : f32 =
  loop x = 0 for i < n do
    x + A[i]

