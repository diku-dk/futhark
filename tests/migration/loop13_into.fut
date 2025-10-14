-- Reads can be delayed into loops given that the number of reads done by the
-- loop remains unchanged.
-- ==
-- structure gpu {
--   /Index 0
--   /Loop/GPUBody/BinOp 1
-- }

def main (A: [10]i64) : [10]i64 =
  let x = A[0]
  let (_, A') =
    loop (x, A) = (x, A)
    for y in A do
      let z = x + y
      in (opaque x, map (+ z) A)
  in A'
