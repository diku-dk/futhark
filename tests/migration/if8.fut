-- Reads can be delayed through if statements.
-- ==
-- structure gpu {
--   /If/True/GPUBody/BinOp 1
--   /GPUBody/BinOp 2
--   /Index 1
-- }

def main (A: [5]i64) (c: bool) : i64 =
  let x = A[0]
  let y = if c then x + 3 else 42
  in x + y + A[1]
