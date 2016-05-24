-- Test a lambda with a free variable.
-- ==
-- input {
--   [1,2,3]
--   1
-- }
-- output {
--   [2, 3, 4]
-- }
fun [int] main([int] a, int y) = map(fn int (int x) => (x+y), a)
