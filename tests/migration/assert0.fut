-- Assertions are considered free to migrate but are not migrated needlessly.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/Index 1
--   /GPUBody/CmpOp 1
--   /GPUBody/Assert 1
-- }

def main (arr: [1]i32) : i32 =
  assert (arr[0] == 42) 1337
