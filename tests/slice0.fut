-- Test of basic slicing.
--
-- ==
-- input { [1,2,3,4,5] 1 3 }
-- output { [2,3] }
-- input { [1,2,3,4,5] 0 5 }
-- output { [1,2,3,4,5] }
-- input { [1,2,3,4,5] 1 1 }
-- output { empty([0]i32) }
-- input { [1,2,3,4,5] 1 0 }
-- error: Index \[1:0\] out of bounds for array of shape \[5\]
-- input { empty([0]i32) 0 1 }
-- error: Index \[0:1\] out of bounds for array of shape \[0\]

def main (as: []i32) (i: i32) (j: i32) : []i32 =
  as[i64.i32 i:i64.i32 j]
