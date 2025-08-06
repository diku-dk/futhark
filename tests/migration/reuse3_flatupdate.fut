-- A statement that reuses memory can be migrated as part of a parent body
-- if none but single elements of the reused memory (updated or aliased) are
-- returned or if the memory source is migrated into the same kernel.
-- ==
-- structure gpu {
--   /GPUBody/If/True/FlatUpdate 1
-- }

import "intrinsics"

def v = [[1i64]]

entry case_if (A: *[5]i64) (x: i64) : [1]i64 =
  if A[4] == 42
  then let B = flat_update_2d A 0 1 1 v
       in #[unsafe] (opaque B)[0:1] :> [1]i64
  else A[0:1] :> [1]i64

-- Compiler limitations prevent the loop cases from being validated.
-- See 'reuse1_update.fut' for details.
