-- Reads can be delayed through loops.
-- ==
-- structure gpu {
--   /GPUBody 2
--   /Loop/GPUBody/BinOp 3
--   /Index 1
-- }

def main (A: [10]i64) : i64 =
  let (a, b) =
    loop (x, y) = (A[0], A[1])
    for i < 10 do
      let z = x + y
      in (z % 22, z * z)
  in a + b
