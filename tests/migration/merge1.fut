-- Migrated statements are moved into GPUBody statements that are combined.
--
-- Can merge non-adjacent GPUBody statements.
-- ==
-- structure gpu {
--   /BinOp 1
--   /GPUBody 1
--   /GPUBody/CmpOp 1
--   /GPUBody/If 1
-- }

def main (A: *[1]i32) (a: i32) : *[1]i32 =
  let x = A[0]
  let b = a * 2
  let y = if x == b then b + 3 else b
  in A with [0] = y
