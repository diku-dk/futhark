-- Whole while loops can be migrated to avoid reading the loop condition but
-- only if doing so can save a read.
-- ==
-- structure gpu {
--   GPUBody 0
-- }

def main (A: [10]i64) : i64 =
  loop x = A[0] while x < 1000 do
    x * x
