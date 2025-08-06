-- Reads can be delayed into kernel bodies and combining operators.
-- ==
-- structure gpu {
--   /Index 0
--   /SegScan 1
--   /SegMap 0
-- }

def main (A: *[10]i64) : [10]i64 =
  let A = A with [0] = 0
  let A = A with [1] = 0
  let B = opaque A
  let x = B[0]
  -- This read can be delayed into op
  let y = B[1]
  -- This read can be delayed into the kernel body

  let op = \a b -> a + b + x
  in scan op 0 (map (+ y) B)
