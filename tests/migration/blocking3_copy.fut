-- Array copying blocks the migration of whole statements.
-- This is to avoid turning a parallel device copy into a sequential operation.
-- ==
-- structure gpu {
--   /If/True/Replicate 1
-- }

def main (A: [5]i64) : [1]i64 =
  if A[0] == 0
     then let B = copy (opaque A)
           in #[unsafe] (opaque B)[0:1]
     else A[1:2] :> [1]i64
