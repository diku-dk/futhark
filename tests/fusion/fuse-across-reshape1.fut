-- ==
-- input {
-- }
-- output {
--   [[2, 4, 6], [8, 10, 12], [14, 16, 18]]
-- }
-- structure {
--   Map 2
-- }
fun main(): [][]i32 =
  let n = 9
  let a = map (+1) (iota(n))
  let b = reshape (3,3) a in
  map  (\(row: []i32): []i32 ->
         map  (\(x: i32): i32 -> x*2) row) b
