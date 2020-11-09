-- ==
-- input {
--   [[1,2,3],[4,5,6]]
-- }
-- output {
--   [[1, 4], [2, 5], [3, 6]]
-- }
-- compiled random input { [10][10]i32 } auto output
-- compiled random input { [1024][4]i32 } auto output
-- compiled random input { [4][1024]i32 } auto output
-- compiled random input { [1024][1024]i32 } auto output
let main(a: [][]i32): [][]i32 =
  transpose a
