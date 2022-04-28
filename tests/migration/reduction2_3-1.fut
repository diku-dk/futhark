-- Only migrate array reads when a reduction can be obtained.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/BinOp 2
--   /Index 1
--   /BinOp 0
-- }

def main (arr: [3]f32) : f32 =
  arr[0] + arr[1] + arr[2]
