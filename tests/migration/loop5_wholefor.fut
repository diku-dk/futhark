-- Whole for loops can be migrated to avoid reading the bound but only if
-- doing so can save a read.
-- ==
-- structure gpu {
--   GPUBody 0
-- }

def main (A: [10]i64) : i64 =
  loop x = 1
  for i < A[0] do
    x * x
