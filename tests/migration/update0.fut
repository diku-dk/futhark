-- Values written by Updates are not required to be available on host.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/Index 1
--   /GPUBody/BinOp 1
--   /Index 0
--   /Update 1
-- }

def main (A: *[9]f32) : *[9]f32 =
  let x = A[4]
  let x' = x / 7
  in A with [4] = x'
