-- Reads can be delayed out of loops and participate in further reductions,
-- even if they are introduced by a for-in loop.
-- ==
-- structure gpu {
--   /DoLoop/Index 0
--   /DoLoop/GPUBody 1
--   /GPUBody 1
--   /Index 1
-- }

def main [n] (A: [n]i64) : i64 =
  let sum =
    loop x = 0 for y in A do
      x + y
   in sum + A[sum % n]
