-- Shape annotation in lambda return type.
--
-- This is intended to avoid shape slices.
-- ==
-- tags { no_opencl no_vulkan }
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

let multiply(a: []i32, n: i32): []i32 =
  loop (a) for i < n-1 do concat a a

let main [m] (a: [m][]i32) (x: i32): [][]i32 =
  let n = m * (2 ** (x-1))
  in map (\(r: []i32): [n]i32  ->
           multiply(r,x)) a
