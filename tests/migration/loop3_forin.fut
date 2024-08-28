-- Reads introduced by a for-in loop can be delayed.
-- ==
-- structure gpu {
--   /Loop/Index 0
--   /Loop/GPUBody 1
--   /GPUBody 1
--   /Index 1
-- }

def main [n] (A: [n]i64) : i64 =
  let sum =
    loop x = 0 for y in A do
      x + y
   in sum + A[sum % n]
