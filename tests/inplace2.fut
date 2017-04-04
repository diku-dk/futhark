-- In-place update with a slice.
--
-- ==
-- input { [1,2,3,4,5] [8,9] 2 }
-- output { [1,2,8,9,5] }
-- input { [1,2,3,4,5] [5,6,7,8,9] 0 }
-- output { [5,6,7,8,9] }
-- input { [1,2,3,4,5] empty(i32) 0 }
-- output { [1,2,3,4,5] }
-- input { [1,2,3,4,5] empty(i32) 1 }
-- output { [1,2,3,4,5] }
-- input { [1,2,3,4,5] empty(i32) 5 }
-- output { [1,2,3,4,5] }
-- input { [1,2,3,4,5] [1,2,3] -1 }
-- error: Assertion.*failed

let main(as: *[n]i32, bs: [m]i32, i: i32): []i32 =
  let as[i:i+m] = bs
  in as
