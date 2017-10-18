-- A mapped scatter should be parallelised.
-- input { [[1,2,3],[4,5,6]] [2,0] [42,1337] }
-- output { [[1337, 2, 42], [1337, 5, 42]] }

let main (xss: *[][]i32) (is: []i32) (vs: []i32) =
  map (\(xs: *[]i32) -> scatter xs is vs) xss
