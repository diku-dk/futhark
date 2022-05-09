-- If statements may be migrated as a whole but only if reads are reduced.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/CmpOp 1
--   /GPUBody/If 1
--   /Index 1
-- }

def main (A: [5]i64) : i64 =
  if A[0] == 0 then A[1] else A[2]
