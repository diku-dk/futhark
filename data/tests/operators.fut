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

fun main(x: int, y: int): (bool,bool) =
  (x > y, x >= y)
