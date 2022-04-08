-- A statement that reuses memory can be migrated as part of a parent body
-- if none but single elements of the reused memory (updated or aliased) are
-- returned.
-- ==
-- structure gpu {
--   /GPUBody/If/True/FlatIndex 0
-- }

import "intrinsics"

-- This fails due to a memory allocation error.

-- def main (A: *[5]i64) (x: i64) : [1]i64 =
--   if A[4] == 42
--      then let A' = flat_index_2d A 0 2 2 2 1
--            in #[unsafe] (opaque A')[0:1, 0] :> [1]i64
--      else A[0:1] :> [1]i64
