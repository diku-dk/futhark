-- Only migrate array reads when a reduction can be obtained.
-- ==
-- structure gpu {
--   GPUBody 0
--   /Index 2
-- }

def main (arr: [3]f32) : (f32, f32) =
  let (a, b) = (arr[0], arr[1])
  let (x, y, z) = (a * b, a + b, a - b)
  in (x * y, y * z)
