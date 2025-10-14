-- Reverse the dimensions of a 3D array.  At one point the code
-- generator thought this was a transposition.
-- ==
-- input { [[[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11]], [[12, 13,
-- 14, 15], [16, 17, 18, 19], [20, 21, 22, 23]]] }
--
-- output { [[[0, 12], [4, 16], [8, 20]], [[1, 13], [5, 17], [9, 21]],
-- [[2, 14], [6, 18], [10, 22]], [[3, 15], [7, 19], [11, 23]]] }

def main [a] [b] [c] (A: [a][b][c]i32) : [c][b][a]i32 =
  A |> map transpose |> transpose |> map transpose
