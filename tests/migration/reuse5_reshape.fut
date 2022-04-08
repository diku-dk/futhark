-- A statement that reuses memory can be migrated as part of a parent body
-- if none but single elements of the reused memory (updated or aliased) are
-- returned.
-- ==
-- structure gpu {
--   /If/True/GPUBody/If/True/Reshape 1
-- }

def main [n] (A: *[n]i64) : [1]i64 =
  if n > 0
     then if #[unsafe] A[0] == 42
          then A :> [1]i64
          else #[unsafe] A[0:1] :> [1]i64
     else [42]
