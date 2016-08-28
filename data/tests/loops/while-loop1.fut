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

fun main(a: []int, i: int, bound: int): []int =
  loop (a) = while a[i] < bound do
    map((+1), a) in
  a
