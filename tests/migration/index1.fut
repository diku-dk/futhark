-- Non-reducible array reads should not be migrated unless to prevent the
-- reading of their indices.
-- ==
-- structure gpu {
--   GPUBody 0
--   /Index 1
-- }

def main (arr: [3][3]i32) : i32 =
  arr[1, 1]
