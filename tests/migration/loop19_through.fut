-- Reads can be delayed through loops.
-- ==
-- structure gpu {
--   /Index 0
--   /Loop/If/True/GPUBody/BinOp 1
--   /Loop/If/False/GPUBody/BinOp 1
-- }

def main (A: *[10]i64) : *[10]i64 =
  let (x, A) =
    loop (x, A) = (A[0], A)
    for i < 10 do
      if i % 4 == 0
      then (x - 1, A)
      else (x + 1, A with [i] = 42)
  in A with [6] = x
