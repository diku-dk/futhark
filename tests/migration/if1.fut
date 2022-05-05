-- If statements may be migrated as a whole but only if reads are reduced.
--
-- Reads can be delayed from any branch, even if only one branch performs a
-- read. This reduces the worst-case number of reads.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/CmpOp 1
--   /GPUBody/If 1
--   /Index 1
-- }

def main (A: [5]i64) : i64 =
  if A[0] == 0 then 42 else A[2]
