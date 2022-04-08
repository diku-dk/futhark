-- Scalar constants written by Updates are NOT transformed. They are already
-- asynchronous.
-- ==
-- structure gpu {
--   GPUBody 0
--   /Update 1
-- }

def main (A: *[5]i32) : *[5]i32 =
  A with [4] = 42
