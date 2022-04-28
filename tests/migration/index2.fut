-- Slice indexes may not be migrated on their own as results from GPUBody
-- constructs are copied, which would change the asymptotic cost of the
-- operation.
-- ==
-- structure gpu {
--   GPUBody 0
--   Index 2
-- }

def main (arr: [3][3]i32) : [3]i32 =
  let i = arr[1, 0]
  in arr[:, i]
