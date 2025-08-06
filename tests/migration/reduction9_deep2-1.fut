-- Only migrate array reads when a reduction can be obtained.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /Index 1
--   /BinOp 0
-- }

def main (arr: [3]f32) : f32 =
  let a = arr[0]
  let b = a + 2
  let c = 4 * b
  let x = arr[1]
  let y = x + 2
  let z = 3 * y
  in c / z
