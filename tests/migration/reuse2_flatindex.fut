-- A statement that reuses memory can be migrated as part of a parent body
-- if none but single elements of the reused memory (updated or aliased) are
-- returned or if the memory source is migrated into the same kernel.
-- ==

-- structure gpu {
--   /GPUBody/If/True/FlatIndex 1
-- }

import "intrinsics"

-- This fails due to a memory allocation error.

-- entry case_if (A: *[5]i64) (x: i64) : [1]i64 =
--   if A[4] == 42
--      then let B = flat_index_2d A 0 2 2 2 1
--            in #[unsafe] (opaque B)[0:1, 0] :> [1]i64
--      else A[0:1] :> [1]i64
