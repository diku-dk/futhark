-- Test that sophisticated operators (such as "greater than") work.
-- ==
-- input {
--   2
--   2
-- }
-- output {
--   false
--   true
-- }

def main (x: i32) (y: i32) : (bool, bool) =
  (x > y, x >= y)
