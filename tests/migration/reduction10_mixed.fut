-- Only migrate array reads when a reduction can be obtained.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /Index 1
--   /BinOp 0
-- }

def main (arr: [3]f32) : f32 =
  let (a, b) = (arr[0], arr[1])
  let c = a * b + a
  let d = -b / 2
  let e = c / arr[2]
  let f = 10 * e
  in f * d
