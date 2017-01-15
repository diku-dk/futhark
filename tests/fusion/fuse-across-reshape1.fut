-- ==
-- input {
-- }
-- output {
--   [[2, 4, 6], [8, 10, 12], [14, 16, 18]]
-- }
-- structure {
--   Map 2
-- }
fun main(): [][]int =
  let n = 9
  let a = map (+1) (iota(n))
  let b = reshape (3,3) a in
  map  (\(row: []int): []int ->
         map  (\(x: int): int -> x*2) row) b
