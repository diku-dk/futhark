-- Assertions are considered free to migrate but are not migrated needlessly.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/Index 1
--   /GPUBody/CmpOp 2
--   /GPUBody/Assert 2
-- }

def main (arr: [1]i32) : i32 =
  let v = arr[0]
  let x = assert (v != 7) 1007
  let y = assert (v != 21) 330
  in x + y
