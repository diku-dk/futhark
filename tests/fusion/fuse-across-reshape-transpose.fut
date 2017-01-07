-- ==
-- input {
-- }
-- output {
--   [[2, 8, 14], [4, 10, 16], [6, 12, 18]]
-- }
-- structure { Map 2 }
fun main(): [][]int =
  let n = 9
  let a = map (+1) (iota(n))
  let b = reshape (3,3) a
  let c = transpose b in
  map  (fn (row: []int): []int  =>
         map  (*2) row) c
