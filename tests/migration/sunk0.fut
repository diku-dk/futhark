-- Array reads are sunk to their deepest possible branch.
-- ==
-- structure gpu {
--   GPUBody 1
--   /Index 0
--   /If/True/Index 1
--   /If/True/GPUBody/Index 1
--   /If/True/GPUBody/If/True/Index 1
--   /If/True/GPUBody/If/False/Index 1
-- }

def main (A: [5]i32) (x: i32) : i32 =
  let y = A[0]
  in if x == 7
     then let z = A[1]
          in if A[2] == 42
             then y
             else z
     else 14
