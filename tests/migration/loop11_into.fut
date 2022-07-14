-- Reads can be delayed into loops given that the number of reads done by the
-- loop remains unchanged.
-- ==
-- structure gpu {
--   /Index 0
--   /DoLoop/GPUBody/BinOp 1
-- }

def main (A: [10]i64) : [10]i64 =
  let x = A[0]
  in loop A for y in A do
       let z = x+y
        in map (+z) A




