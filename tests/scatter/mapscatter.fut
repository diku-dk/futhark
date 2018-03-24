-- You can map a scatter.
-- ==
-- input { [[1,2,3],[4,5,6]] [[1,-1,-1],[-1,0,1]] [[0,0,0],[0,0,0]] }
-- output { [[1,0,3],[0,0,6]] }

let main (as: *[][]i32) (is: [][]i32) (vs: [][]i32) =
  map (\(x: *[]i32, y, z) -> scatter x y z) (zip as is vs)
