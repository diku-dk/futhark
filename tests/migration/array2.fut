-- Array literals are migrated if they contain free scalar variables.
--
-- Arrays with non-primitive rows are not be migrated.
-- ==
-- structure gpu {
--   GPUBody 0
--   ArrayLit 3
-- }

def main (i: i64) (v: [2]i32) : i32 =
  let xs = [[0, 1], [2, 3], v]
  in xs[i % 3, i % 2]
