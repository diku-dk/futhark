-- Shape annotation in lambda return type.
--
-- This is intended to avoid shape slices.
-- ==
-- tags { no_opencl no_cuda no_cudatc no_hip no_pyopencl }
-- input {
--   [[1,2,3],
--    [4,5,6],
--    [7,8,9]]
--   3i64
-- }
-- output {
--   [[1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3],
--    [4, 5, 6, 4, 5, 6, 4, 5, 6, 4, 5, 6],
--    [7, 8, 9, 7, 8, 9, 7, 8, 9, 7, 8, 9]]
-- }

def multiply (a: []i32) (x: i64) (n: i64) : [n]i32 =
  (loop (a) for i < x - 1 do concat a a) :> [n]i32

def main [m] (a: [m][]i32) (x: i64) : [][]i32 =
  let n = m * (2 ** (x - 1))
  in map (\(r: []i32) : [n]i32 ->
            multiply r x n)
         a
