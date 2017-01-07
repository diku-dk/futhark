-- Test integer semantics - overflow and the like.
--
-- This relies on integers being 32 bit and signed, and shifts doing
-- sign extension.
--
-- ==
-- input {
--   2147483647
--   -2147483648
-- }
-- output {
--   2147483647
--   -2147483648
--   2147483647
--   -1073741824
-- }

fun main(a: int, b: int): (int, int, int, int) =
  (a, a+1, b-1, b>>1)
