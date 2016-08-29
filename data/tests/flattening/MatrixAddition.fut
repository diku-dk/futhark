-- ==
-- input {
--   [[1,2,3],[1,2,3]]
--   [[3,2,1],[6,7,8]]
-- }
-- output {
--   [[4,4,4],[7,9,11]]
-- }
fun addRows (xs: []int, ys: []int): []int =
  map (+) (zip  xs ys)

fun addMatricies (a: [][]int, b: [][]int): [][]int =
  map  addRows (zip  a b)

fun main(a: [][]int, b: [][]int): [][]int =
  addMatricies(a,b)
