-- A statement that reuses memory can be migrated as part of a parent body
-- if none but single elements of the reused memory (updated or aliased) are
-- returned or if the memory source is migrated into the same kernel.
-- ==
-- structure gpu {
--   /GPUBody/If/True/Rearrange 1
-- }

def main (A: [3][2]i64) (x: i64) : [1]i64 =
  if A[0,0] == 42
     then let A' = transpose (opaque A)
           in #[unsafe] (opaque A')[0, 0:1] :> [1]i64
     else A[0, 0:1] :> [1]i64
