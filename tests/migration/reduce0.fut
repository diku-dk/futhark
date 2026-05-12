-- Neutral elements are made available on host.
-- ==
-- structure gpu {
--   /Index 1
--   /SegRed 1
-- }

def main (A: *[10]f32) : [1]f32 =
  let A = A with [0] = 0
  let B = opaque A
  let ne = B[0]
  in [reduce (+) ne B]
