-- Non-reducible array reads should not be migrated unless to prevent the
-- reading of their indices.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/Index 2
--   /Index 1
-- }

def main (arr: [3][3]i32) : i32 =
  let i = arr[1, 0]
  in arr[0, i]
