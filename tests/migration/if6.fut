-- Reads can be delayed out of if statements, even if only one branch performs
-- a read. This reduces the worst-case number of reads.
-- ==
-- structure gpu {
--   /If 1
--   /If/True/Index 0
--   /GPUBody/BinOp 1
--   /Index 1
-- }

def main (A: [5]i64) (c: bool) : i64 =
  let x = if c then A[1] else 42
  in x + A[3]
