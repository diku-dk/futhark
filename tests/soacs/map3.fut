-- Test a lambda with a free variable.
-- ==
-- input {
--   [1,2,3]
--   1
-- }
-- output {
--   [2, 3, 4]
-- }
def main (a: []i32) (y: i32) : []i32 = map (\(x: i32) : i32 -> (x + y)) a
