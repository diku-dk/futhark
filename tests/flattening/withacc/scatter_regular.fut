-- Completely regular case.
-- ==
-- input { [[1,2,3], [4,5,6], [7,8,9]]
--         [[0i64, 2], [-1i64, 0], [1i64,0]]
--         [[1,2], [3,4], [5,6]]
--       }
-- output { [[1, 2, 2], [4, 5, 6], [6, 5, 9]] }

def main =
  map3 \(xs: []i32) (is: []i64) (vs: []i32) ->
    scatter (copy xs) is vs
