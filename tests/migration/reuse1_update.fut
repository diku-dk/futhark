-- A statement that reuses memory can be migrated as part of a parent body
-- if none but single elements of the reused memory (updated or aliased) are
-- returned or if the memory source is migrated into the same kernel.
-- ==
-- structure gpu {
--   /GPUBody/If/True/Update 1
--   /GPUBody/Loop/Update 0
-- }

entry case_if (A: *[5]i64) (x: i64) : [1]i64 =
  if A[0] == 42
     then let B = #[unsafe] A with [x%5] = 0
           in #[unsafe] (opaque B)[0:1] :> [1]i64
     else A[0:1] :> [1]i64

-- Compiler limitations prevent these cases from being validated.
--
-- We cannot consume something outside the loops; allocating inside the
-- (migrated) loops causes allocation errors; introducing a multi-element
-- array as a loop parameter disables the optimization; and updating a
-- single-element array loop parameter is replaced with a replicate, which
-- also causes an allocation error.

-- entry case_while (A: *[5]i64) : [1]i64 =
--   let (_, C) =
--     loop (x, A') = (0, A[0:1]) while A'[1] != x do
--       let B = #[unsafe] [0, 1, x, 3, 4] with [x%5] = 0
--        in (x+1, #[unsafe] (opaque B)[0:1] :> [1]i64)
--    in C

-- entry case_for (A: *[5]i64) : [1]i64 =
--   let (_, C) =
--     loop (x, _) = (0, A[0:1]) for i < A[1] do
--       let B = #[unsafe] [0, 1, x, 3, 4] with [x%5] = 0
--        in (x+1, #[unsafe] (opaque B)[0:1] :> [1]i64)
--    in C
