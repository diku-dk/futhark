-- ==
-- input {
--   [[1,2,3],[4,5,6]]
-- }
-- output {
--   [[1, 4], [2, 5], [3, 6]]
-- }
-- input {
--   empty([0][1]i32)
-- }
-- output {
--   empty([1][0]i32)
-- }
-- compiled random input { [10][10]i32 } auto output
-- compiled random input { [1024][4]i32 } auto output
-- compiled random input { [4][1024]i32 } auto output
-- compiled random input { [1024][1024]i32 } auto output
def main [n] [m] (a: [n][m]i32) : [m][n]i32 =
  transpose a
