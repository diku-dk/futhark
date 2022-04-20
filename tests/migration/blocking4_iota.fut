-- Iotas blocks the migration of whole statements.
-- This is to avoid turning a parallel computation into a sequential operation.
-- ==
-- structure gpu {
--   GPUBody 0
--   /If/True/Iota 1
-- }

def main (A: [5]i64) : [1]i64 =
  if A[4] == 42
     then let B = iota A[0]
           in #[unsafe] (opaque B)[0:1] :> [1]i64
     else A[0:1] :> [1]i64
