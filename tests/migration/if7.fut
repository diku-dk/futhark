-- Reads can be delayed into if statements.
-- ==
-- structure gpu {
--   /Index 0
--   /If/True/GPUBody/BinOp 1
-- }

def main (A: *[5]i64) (c: bool) : *[5]i64 =
  let x = A[0]
  let A' = if c then A with [0] = x + 1 else A
  in A' with [2] = x
