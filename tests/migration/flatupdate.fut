-- Flat updates may not be migrated on their own as results from GPUBody
-- constructs are copied, which would change the asymptotic cost of the
-- operation.
-- ==
-- structure gpu {
--   GPUBody 0
--   FlatUpdate 1
-- }

import "intrinsics"

-- This fails due to a memory allocation error.

def v = [[1i64]]

def main (A: *[5]i64) : *[5]i64 =
  let x = A[0]
  in flat_update_2d A x 1 1 v
