-- Assertions are considered free to migrate but are not migrated needlessly.
--
-- This test is a variation of the 'index0.fut' test.
-- ==
-- structure gpu {
--   /Assert 1
--   /GPUBody 1
--   /GPUBody/Assert 1
--   /GPUBody/Index 2
--   /Index 1
-- }

def main (arr: []i64) : i64 =
  let i = arr[0]
  in arr[i]
