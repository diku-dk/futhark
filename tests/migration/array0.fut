-- Array literals are migrated if they contain free scalar variables.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/ArrayLit 1
--   /ArrayLit 0
-- }

def main (i: i64) (v: i32) : i32 =
  let xs = [0, 1, 2, 3, v]
  in xs[i % 5]
