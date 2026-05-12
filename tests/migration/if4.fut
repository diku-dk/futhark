-- If statements may be migrated as a whole but only if reads are reduced.
--
-- A read is saved by migrating the 'if' as the second tuple value is not used
-- on host.
-- ==
-- structure gpu {
--   /GPUBody/CmpOp 1
--   /GPUBody/If/False/Index 1
--   /GPUBody/BinOp 1
--   /Index 1
-- }

def main (A: *[5]i64) : i64 =
  let (x, y) = if A[0] == 0 then (A[1], 42) else (A[2], 1337)
  let A' = map (+ y) A
  in x + A'[4]
