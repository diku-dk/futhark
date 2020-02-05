-- Slicing a multidimensional array across the outer dimension.
--
-- ==
-- input { [[1,2,3],[4,5,6]] 1 3 }
-- output { [[2,3],[5,6]] }
-- input { [[1,2,3],[4,5,6]] 0 3 }
-- output { [[1,2,3],[4,5,6]] }
-- input { [[1,2,3],[4,5,6]] 1 1 }
-- output { empty([2][0]i32) }
-- input { [[1,2,3],[4,5,6]] 1 0 }
-- error: Index \[0:2, 1:0\] out of bounds for array of shape \[2\]\[3\].

let main [n][m] (as: [n][m]i32) (i: i32) (j: i32): [n][]i32 =
  as[0:n,i:j]
