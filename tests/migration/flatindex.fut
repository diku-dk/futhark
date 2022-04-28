-- Flat indexes may not be migrated on their own as results from GPUBody
-- constructs are copied, which would change the asymptotic cost of the
-- operation.
-- ==
-- structure gpu {
--   GPUBody 0
--   FlatIndex 1
-- }

import "intrinsics"

-- This fails due to a memory allocation error.

def main (A: [5]i64) : [2][2]i64 =
  flat_index_2d A 0 2 2 2 A[0]
