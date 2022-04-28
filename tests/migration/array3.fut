-- Array literals are migrated if they contain free scalar variables.
--
-- Arrays with non-primitive rows are not be migrated. This is to avoid turning
-- a parallel device copy into a sequential operation.
-- ==
-- structure gpu {
--   GPUBody 0
-- }

def main [n] (A: [n]i32) : [1][n]i32 =
  [A]
