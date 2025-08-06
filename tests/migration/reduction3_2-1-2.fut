-- Only migrate array reads when a reduction can be obtained.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/BinOp 1
--   /Index 1
--   /BinOp 2
-- }

def main (arr: [3]f32) : (f32, f32) =
  let (a, b) = (arr[0], arr[1])
  let x = a * b
  in (x + 1, x - 1)
