-- Multi-element array slice updates block the migration of whole statements.
-- This is to avoid turning a parallel device copy into a sequential operation.
-- ==
-- structure gpu {
--   /If/True/Update 1
-- }

entry case_if (A: *[5]i64) (x: i64) : i64 =
  if A[0] == 0
  then let B = A with [0:2] = [4, 2]
       in #[unsafe] (opaque B)[x % length B]
  else A[1]
