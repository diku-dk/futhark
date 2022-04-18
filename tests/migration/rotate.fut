-- Rotates cannot be migrated to prevent operands from being read.
-- ==
-- structure gpu {
--   GPUBody 0
--   Rotate 1
-- }

def main (A: [5]i64) : [5]i64 =
  rotate A[0] A
