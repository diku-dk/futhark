-- ==
-- input {
--   [[1,2,3],[1,2,3]]
--   [[3,2,1],[6,7,8]]
-- }
-- output {
--   [[4,4,4],[7,9,11]]
-- }
fun addRows (xs: []i32, ys: []i32): []i32 =
  map (+) xs ys

fun addMatricies (a: [][]i32, b: [][]i32): [][]i32 =
  map  addRows (zip  a b)

fun main(a: [][]i32, b: [][]i32): [][]i32 =
  addMatricies(a,b)
