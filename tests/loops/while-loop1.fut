-- Test a while loop that has an array merge variable and checks it in
-- its condition.
-- ==
-- input {
--   [1,2,3,4,5,6]
--   3
--   10
-- }
-- output {
--   [7, 8, 9, 10, 11, 12]
-- }

def main (a: []i32) (i: i32) (bound: i32) : []i32 =
  loop (a) while a[i] < bound do map (+ 1) a
