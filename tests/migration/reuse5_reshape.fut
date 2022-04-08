-- A statement that reuses memory can be migrated as part of a parent body
-- if none but single elements of the reused memory (updated or aliased) are
-- returned.
-- ==
-- structure gpu {
--   /GPUBody/If/True/Reshape 1
-- }

def main (A: *[5]i64) (x: i64) : [1]i64 =
  if A[4] == 42
     then let A' = [[1, x], [3, 4]] -- is optimized as a reshape of [1,x,3,4].
           in #[unsafe] (opaque A')[0:1, 0] :> [1]i64
     else A[0:1] :> [1]i64
