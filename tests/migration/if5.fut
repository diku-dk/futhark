-- Reads can be delayed out of if statements.
-- ==
-- structure gpu {
--   /If 1
--   /If/True/Index 0
--   /If/False/Index 0
--   /GPUBody/BinOp 1
--   /Index 1
-- }

def main (A: [5]i64) (c: bool) : i64 =
  let x = if c then A[1] else A[2]
  in x + A[3]
