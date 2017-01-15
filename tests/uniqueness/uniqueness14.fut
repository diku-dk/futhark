-- ==
-- input {
--   42
-- }
-- output {
--   [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- }
fun f(b_1: *[]i32): *[]i32 =
  copy(iota(10))

fun main(n: i32): []i32 =
  let a = iota(n)
  let x = if n == 0 then a else f(a) in
  x
