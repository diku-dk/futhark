-- ==
-- input {
--   [1,2,3,4]
--   [5,6,7,8]
-- }
-- output {
--   [6,8,10,12]
-- }
fun []int main([]int xs, []int ys) =
  map(+, zip (xs,ys))
