-- Any operation that normally is parallel does not block migration of parent
-- statements if it produces an array of just a single element.
-- ==

-- structure gpu {
--   GPUBody/If/True/Replicate 1
-- }

-- This fails due to a memory allocation error.

-- def main (A: [1]i64) : [1]i64 =
--   if A[4] == 42
--      then copy (opaque A)
--      else A
