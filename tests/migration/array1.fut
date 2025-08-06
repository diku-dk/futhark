-- Array literals are migrated if they contain free scalar variables.
-- ==
-- structure gpu {
--   GPUBody 0
--   ArrayLit 1
-- }

def main (i: i64) : i32 =
  let xs = [0, 1, 2, 3, 4]
  in xs[i % 5]
