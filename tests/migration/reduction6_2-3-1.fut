-- Only migrate array reads when a reduction can be obtained.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/BinOp 5
--   /Index 1
--   /BinOp 0
-- }

def main (arr: [3]f32) : f32 =
  let (a, b) = (arr[0], arr[1])
  let (x, y, z) = (a * b, a + b, a - b)
  in x * y * z
