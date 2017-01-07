-- Test a simple map, that might be irregular based on input data.
-- ==
-- tags { no_opencl }
-- input {
--   [8,8,8,8]
-- }
-- output {
--   [[0, 1, 2, 3, 4, 5, 6, 7], [0, 1, 2, 3, 4, 5, 6, 7], [0, 1, 2, 3, 4, 5, 6, 7], [0, 1, 2, 3, 4, 5, 6, 7]]
-- }
fun main(a: []int): [][]int =
  map (fn (n: int): []int  => iota(n)) a
