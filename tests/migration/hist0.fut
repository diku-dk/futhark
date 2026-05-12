-- Neutral elements are made available on host.
-- ==
-- structure gpu {
--   /Index 1
--   /SegHist 1
-- }

def main (A: *[10]i32) : *[10]i32 =
  let A = A with [0] = 0
  let B = opaque A
  let ne = B[0]
  in reduce_by_index B (+) ne [4, 2] [1, 0]
