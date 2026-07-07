-- A statement that reuses memory can be migrated as part of a parent body
-- if none but single elements of the reused memory (updated or aliased) are
-- returned or if the memory source is migrated into the same kernel.
-- ==
-- structure gpu {
--   /GPUBody/If/True/Index 2
--   /GPUBody/Loop/Index 5
-- }

entry case_if (A: *[5]i64) (x: i64) : [1]i64 =
  if A[0] == 42
  then let B = #[unsafe] A[x % 3:x % 3 + 2]
       in #[unsafe] (opaque B)[0:1] :> [1]i64
  else A[0:1] :> [1]i64

entry case_while (A: [5]i64) : [1]i64 =
  let (_, C) =
    loop (x, A') = (0, A[0:1])
    while A'[0] != x do
      let B = #[unsafe] A[x % 3:x % 3 + 2]
      in (x + 1, #[unsafe] (opaque B)[0:1] :> [1]i64)
  in C

entry case_for (A: [5]i64) : [1]i64 =
  let (_, C) =
    loop (x, _) = (0, A[0:1])
    for i < A[0] do
      let B = #[unsafe] A[x % 3:x % 3 + 2]
      in (x + 1, #[unsafe] (opaque B)[0:1] :> [1]i64)
  in C
