-- Only migrate array reads when a reduction can be obtained.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/Index 3
--   /GPUBody/BinOp 4
--   /Index 2
--   /BinOp 6
-- }

def main (arr: [3]f32) : (f32, f32) =
  let (a, b, c) = (arr[0], arr[1], arr[2])
  let (x, y) = (a + b + c, a - b - c)
  let (d, e, f, g) = (x + y, x - y, x * y, x / y)
  in (d % e, f + g)
