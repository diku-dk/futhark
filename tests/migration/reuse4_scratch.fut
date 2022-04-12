-- A statement that reuses memory can be migrated as part of a parent body
-- if none but single elements of the reused memory (updated or aliased) are
-- returned.
-- ==

-- structure gpu {
--   /GPUBody/If/True/Scratch 1
--   /GPUBody/DoLoop/Scratch 1
-- }

-- This fails due to a memory allocation error.

-- entry case1 (A: *[5]i64) : [1]i64 =
--   if A[4] == 42
--      then let A' = #[sequential] map (+1) A
--            in #[unsafe] (opaque A')[0:1] :> [1]i64
--      else A[0:1] :> [1]i64

-- entry case2 (A: *[5]i64) : [1]i64 =
--   loop A' = A[0:1] for i < A[1] do
--     let B = [0, 1, 2, 3, A'[0]]
--     let C = #[sequential] map (+i) B
--     let idx = i%5
--      in C[idx:idx+1] :> [1]i64