-- Neutral elements are made available on host.
-- ==
-- structure gpu {
--   /Index 1
--   /SegScan 1
-- }

def main (A: *[10]f32) : [10]f32 =
  let A = A with [0] = 0
  let B = opaque A
  let ne = B[0]
  in scan (+) ne B
