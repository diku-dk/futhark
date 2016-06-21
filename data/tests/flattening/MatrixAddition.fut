-- ==
-- input {
--   [[1,2,3],[1,2,3]]
--   [[3,2,1],[6,7,8]]
-- }
-- output {
--   [[4,4,4],[7,9,11]]
-- }
fun []int addRows ([]int xs, []int ys) =
  map(+, zip (xs,ys))

fun [][]int addMatricies ([][]int a, [][]int b) =
  map (addRows, zip (a,b))

fun [][]int main([][]int a, [][]int b) =
  addMatricies(a,b)
