-- Sinking should be as deep as possible.
-- ==
-- structure gpu {
--   /GPUBody/Index 1
--   /GPUBody/If/False/If/True/Index 1
--   /GPUBody/If/False/If/False/If/True/Index 1
--   /GPUBody/If/False/If/False/If/False/Index 2
-- }

def main (arr: [](i32, i32, i32, i32, i32)) =
  let (a, b, c, d, e) = arr[0]
  in if a == 0
     then 0
     else if a == 1
     then b
     else if a == 2
     then c
     else d + e
