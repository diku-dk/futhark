-- Array concatenation blocks the migration of whole statements.
-- This is to avoid turning a parallel device copy into a sequential operation.
-- ==
-- structure gpu {
--   /If/True/Concat 1
--   /Loop/Concat 2
-- }

entry case_if (A: [5]i64) (x: i64) : i64 =
  if A[0] == 0
     then let B = concat A (opaque A)
           in #[unsafe] (opaque B)[x%10]
     else A[1]

entry case_while (A: [5]i64) : i64 =
  loop x = A[0] while x < 1000 do
    let B = concat A (opaque A)
     in #[unsafe] (opaque B)[x%10]

entry case_for (A: [5]i64) : i64 =
  loop x = 0 for i < A[0] do
    let B = concat A (opaque A)
     in #[unsafe] (opaque B)[x%10]
