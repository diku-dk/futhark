-- A statement that reuses memory can be migrated as part of a parent body
-- if none but single elements of the reused memory (updated or aliased) are
-- returned.
-- ==
-- structure gpu {
--   /GPUBody/If/True/Index 2
-- }

def main (A: *[5]i64) (x: i64) : [1]i64 =
  if A[4] == 42
     then let A' = #[unsafe] A[:1+x%4]
           in #[unsafe] (opaque A')[0:1] :> [1]i64
     else A[0:1] :> [1]i64

