-- See if we can access an array with a stride.
--
-- ==
-- input { [0,1,2,3,4,5,6,7,8,9] 4 9 2 } output { [4,6,8] }
-- input { [0,1,2,3,4,5,6,7,8,9] 9 4 -2 } output { [9,7,5] }
-- input { [0,1,2,3,4,5,6,7,8,9] 9 -10 -2 } error: out of bounds
-- input { [0,1,2,3,4,5,6,7] 7 9 2 } output { [7] }

def main (as: []i32) (i: i32) (j: i32) (s: i32) : []i32 =
  as[i64.i32 i:i64.i32 j:i64.i32 s]
