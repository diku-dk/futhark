-- In-place update with a slice.
--
-- ==
-- input { [1,2,3,4,5] [8,9] 2i64 }
-- output { [1,2,8,9,5] }
-- input { [1,2,3,4,5] [5,6,7,8,9] 0i64 }
-- output { [5,6,7,8,9] }
-- input { [1,2,3,4,5] empty([0]i32) 0i64 }
-- output { [1,2,3,4,5] }
-- input { [1,2,3,4,5] empty([0]i32) 1i64 }
-- output { [1,2,3,4,5] }
-- input { [1,2,3,4,5] empty([0]i32) 5i64 }
-- output { [1,2,3,4,5] }
-- input { [1,2,3,4,5] [1,2,3] -1i64 }
-- error: Error

def main [n] [m] (as: *[n]i32) (bs: [m]i32) (i: i64) : []i32 =
  let as[i:i + m] = bs
  in as
