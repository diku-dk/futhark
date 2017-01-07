-- Test a lambda with a free variable.
-- ==
-- input {
--   [1,2,3]
--   1
-- }
-- output {
--   [2, 3, 4]
-- }
fun main(a: []int, y: int): []int = map (fn (x: int): int  => (x+y)) a
