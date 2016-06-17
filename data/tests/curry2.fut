-- Curry a simple function.
-- ==
-- input {
--   [8,5,4,3,2,1]
-- }
-- output {
--   [9,6,5,4,3,2]
-- }

fun int add(int x, int y) = x + y

fun []int main([]int a) =
  map(add(1), a)
