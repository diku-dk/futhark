-- Test that sophisticated operators (such as "greater than") work.
-- ==
-- input {
--   2
--   2
-- }
-- output {
--   False
--   True
-- }

fun (bool,bool) main(int x, int y) =
  (x > y, x >= y)
