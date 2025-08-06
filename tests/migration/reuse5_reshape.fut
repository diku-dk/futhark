-- A statement that reuses memory can be migrated as part of a parent body
-- if none but single elements of the reused memory (updated or aliased) are
-- returned or if the memory source is migrated into the same kernel.
-- ==
-- structure gpu {
--   /If/True/GPUBody/If/True/Reshape 1
--   /GPUBody/Loop/Reshape 2
-- }

-- These programs are artificial.
-- Most natural alternatives cannot be validated due to compiler limitations.

entry case_if [n] (A: *[n]i64) : [1]i64 =
  if n > 0
  then if #[unsafe] A[0] == 42
       then (opaque A) :> [1]i64
       else #[unsafe] A[0:1] :> [1]i64
  else [42]

entry case_while (A: []i64) : [1]i64 =
  let (_, C) =
    loop (x, A') = (0, A[0:1])
    while A'[0] != x do
      (x + 1, (opaque A) :> [1]i64)
  in C

entry case_for (A: [5]i64) : [1]i64 =
  let (_, C) =
    loop (x, _) = (0, A[0:1])
    for i < A[0] do
      (x + 1, (opaque A) :> [1]i64)
  in C
