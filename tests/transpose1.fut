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

-- Transpositions of non-contiguous arrays, which are copied with the
-- transposition kernels.  The datasets exercise each of the GPU
-- transposition kernels.

-- ==
-- entry: tr_slice
-- input { [[1,2,3],[4,5,6],[7,8,9]] } output { [[5,8],[6,9]] }
-- compiled random input { [600][5]i32 } auto output
-- compiled random input { [5][600]i32 } auto output
-- compiled random input { [130][70]i32 } auto output

entry tr_slice [n] [m] (xss: [n][m]i32) =
  copy (transpose xss)[1:, 1:]

-- ==
-- entry: tr_reversed
-- input { [[1,2,3],[4,5,6]] } output { [[4,1],[5,2],[6,3]] }
-- compiled random input { [600][5]i32 } auto output
-- compiled random input { [130][70]i32 } auto output

entry tr_reversed [n] [m] (xss: [n][m]i32) =
  copy (transpose (reverse xss))

-- ==
-- entry: maptr_slice
-- input { [[[1,2,3],[4,5,6]],[[7,8,9],[10,11,12]]] }
-- output { [[[5],[6]],[[11],[12]]] }
-- compiled random input { [4][40][50]i32 } auto output
-- compiled random input { [6][3][20]i32 } auto output

entry maptr_slice [k] [n] [m] (xsss: [k][n][m]i32) =
  copy (map transpose xsss[:, 1:, 1:])

-- ==
-- entry: tr_into_slice
-- input { [[1,2,3],[4,5,6]] [[1,2],[3,4],[5,6]] }
-- output { [[1,2,3],[4,4,6]] }
-- compiled random input { [70][130]i32 [130][70]i32 } auto output

entry tr_into_slice [n] [m] (xss: *[n][m]i32) (yss: [m][n]i32) =
  xss with [1:, 1:] = transpose yss[1:, 1:]
