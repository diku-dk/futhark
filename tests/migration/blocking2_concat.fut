-- Array concatenation blocks the migration of whole statements.
-- This is to avoid turning a parallel device copy into a sequential operation.
-- ==
-- structure gpu {
--   GPUBody 0
--   /If/True/Concat 1
-- }

def main (A: [5]i64) : [1]i64 =
  if A[4] == 42
     then let B = concat A A
           in #[unsafe] (opaque B)[0:1] :> [1]i64
     else A[0:1] :> [1]i64
