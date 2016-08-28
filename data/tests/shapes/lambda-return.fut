-- Shape annotation in lambda return type.
--
-- This is intended to avoid shape slices.
-- ==
-- tags { no_opencl }
-- input {
--   [[1,2,3],
--    [4,5,6],
--    [7,8,9]]
--   3
-- }
-- output {
--   [[1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3],
--    [4, 5, 6, 4, 5, 6, 4, 5, 6, 4, 5, 6],
--    [7, 8, 9, 7, 8, 9, 7, 8, 9, 7, 8, 9]]
-- }

fun multiply(a: []int, n: int): []int =
  if n == 1 then a else multiply(concat(a,a), n-1)

fun main(a: [m][]int, x: int): [][]int =
  let n = m * (2 ** (x-1))
  in map (fn (r: []int): [n]int  =>
           multiply(r,x)) a
