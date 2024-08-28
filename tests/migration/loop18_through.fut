-- Reads can be delayed through loops.
-- ==
-- structure gpu {
--   /Index 1
--   /GPUBody/Loop 1
-- }

def main (A: [10]i64) : i64 =
  let x = A[0]
  let y = A[1]
  in loop z = 0 while z < 1000 do
       (x+z)+y
