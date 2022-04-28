-- Assertions are considered free to migrate but are not migrated needlessly.
-- ==
-- structure gpu {
--   GPUBody 0
--   /Assert 1
-- }

def main (fail: bool) : i32 =
  assert fail 800
