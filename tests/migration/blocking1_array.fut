-- Arrays with non-primitive rows block the migration of whole statements.
-- This is to avoid turning a parallel device copy into a sequential operation.
-- ==
-- structure gpu {
--   GPUBody 0
--   /If/True/ArrayLit 1
-- }

def main (A: [5]i64) : [1]i64 =
  if A[4] == 42
     then let B = [A, opaque A]
           in #[unsafe] (opaque B)[0, 0:1] :> [1]i64
     else A[0:1] :> [1]i64
