-- Reads can be delayed into kernel bodies and combining operators.
-- ==
-- structure gpu {
--   /Index 0
-- }

import "intrinsics"

def main (A: *[10]i32) : *[10]i32 =
  let A = A with [0] = 0
  let A = A with [1] = 1
  let B = opaque A
  let x = B[0]
  -- This read can be delayed into op
  let y = B[1]
  -- This read can be delayed into f

  let op = \a b -> a + b + x
  let f = \(acc: *acc ([]i32)) i -> write acc i y
  in reduce_by_index_stream B op 0 f (iota 10)
