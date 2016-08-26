-- Simplest test of while loops.
-- ==
-- input {
--   1
--   9
-- }
-- output {
--   16
-- }

fun main(x: int, bound: int): int =
  loop (x) = while x < bound do x * 2
  in x
