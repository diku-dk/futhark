-- A statement that reuses memory can be migrated as part of a parent body
-- if none but single elements of the reused memory (updated or aliased) are
-- returned or if the memory source is migrated into the same kernel.
-- ==

-- structure gpu {
--   /GPUBody/If/True/Rotate 1
--   /GPUBody/DoLoop/Rotate 1
-- }

-- These fail due to memory allocation errors.

-- entry case_if (A: [5]i64) : [1]i64 =
--   if A[0] == 42
--      then let B = rotate 1 (opaque A)
--            in #[unsafe] (opaque B)[0:1] :> [1]i64
--      else A[0:1] :> [1]i64

-- entry case_for (A: [5]i64) : [1]i64 =
--   loop _ = A[0:1] for i < A[1] do
--     let B = rotate i (opaque A)
--      in B[0:1]
