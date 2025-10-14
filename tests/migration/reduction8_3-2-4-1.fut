-- Only migrate array reads when a reduction can be obtained.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /Index 1
--   /BinOp 0
-- }

def main (arr: [3]f32) : f32 =
  let (a, b, c) = (arr[0], arr[1], arr[2])
  let (x, y) = (a + b + c, a - b - c)
  let (d, e, f, g) = (x + y, x - y, x * y, x / y)
  in d * e * f * g
