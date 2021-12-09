-- You can map a scatter (sort of).
-- ==
-- input { [[1,2,3],[4,5,6]] [[1,-1,-1],[-1,0,1]] [[0,0,0],[0,0,0]] }
-- output { [[1,0,3],[0,0,6]] }

def main (as: [][]i32) (is: [][]i32) (vs: [][]i32) =
  map3 (\x y z -> scatter (copy x) (map i64.i32 y) z) as is vs
