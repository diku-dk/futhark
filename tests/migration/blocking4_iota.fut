-- Iotas blocks the migration of whole statements.
-- This is to avoid turning a parallel computation into a sequential operation.
-- ==
-- structure gpu {
--   /If/True/Iota 1
--   /Loop/Iota 2
-- }

entry case_if (A: [5]i64) (x: i64) : i64 =
  if A[0] == 0
     then let B = iota A[1]
           in #[unsafe] (opaque B)[x % length B]
     else A[1]

entry case_while (A: [5]i64) (x: i64) : i64 =
  loop y = A[0] while y < 1000 do
    let B = iota y
     in #[unsafe] (opaque B)[x % length B]

entry case_for (A: [5]i64) (x: i64) : i64 =
  loop y = 0 for i < A[0] do
    let B = iota y
     in #[unsafe] (opaque B)[x % length B]
