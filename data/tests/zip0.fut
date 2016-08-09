-- Basic test that zip doesn't totally mess up everything.
-- ==
-- input {
--   [1,2,3]
--   [4,5,6]
-- }
-- output {
--   [1, 2, 3]
--   [4, 5, 6]
-- }
fun [](int,int) main([]int a, []int b) =
  zip(a,b)
