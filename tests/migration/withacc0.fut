-- Neutral elements are made available on host.
-- ==
-- structure gpu {
--   /Index 1
-- }

import "intrinsics"

def f (acc: *acc ([]i32)) i =
  let acc = write acc i 1
  let acc = write acc (i + 1) 1
  in acc

def main (A: *[10]i32) : *[10]i32 =
  let A = A with [0] = 0
  let B = opaque A
  let ne = B[0]
  in reduce_by_index_stream B (+) ne f (iota 10)
