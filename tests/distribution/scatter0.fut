-- A mapped scatter should be parallelised.
-- input { [[1,2,3],[4,5,6]] [2,0] [42,1337] }
-- output { [[1337, 2, 42], [1337, 5, 42]] }

def main (xss: *[][]i32) (is: []i64) (vs: []i32) =
  map (\(xs: []i32) -> scatter (copy xs) is vs) xss
