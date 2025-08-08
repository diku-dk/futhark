-- Reads can be delayed through loops.
-- ==
-- structure gpu {
--   /Index 1
--   /Loop/GPUBody/BinOp 2
-- }

def main (A: [10]i64) : i64 =
  let x = A[0]
  let y = A[1]
  in loop z = 0
     for i < 10 do
       (x + z) + y
