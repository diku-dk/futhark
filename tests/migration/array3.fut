-- Array literals are migrated if they contain free scalar variables.
--
-- Arrays with non-primitive rows are not be migrated.
-- ==
-- structure gpu {
--   GPUBody 0
-- }

def main [n] (A: [n]i32) : [1][n]i32 =
  [A]
