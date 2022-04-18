-- A statement that reuses memory can be migrated as part of a parent body
-- if none but single elements of the reused memory (updated or aliased) are
-- returned or if the memory source is migrated into the same kernel.
-- ==
-- structure gpu {
--   /GPUBody/If/True/Update 1
-- }

def main (A: *[5]i64) (x: i64) : [1]i64 =
  if A[4] == 42
     then let A' = #[unsafe] A with [x%5] = 0
           in #[unsafe] (opaque A')[0:1] :> [1]i64
     else A[0:1] :> [1]i64

