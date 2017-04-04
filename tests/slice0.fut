-- Test of basic slicing.
--
-- ==
-- input { [1,2,3,4,5] 1 3 }
-- output { [2,3] }
-- input { [1,2,3,4,5] 0 5 }
-- output { [1,2,3,4,5] }
-- input { [1,2,3,4,5] 1 1 }
-- output { empty(i32) }
-- input { [1,2,3,4,5] 1 0 }
-- error: Assertion.*failed

let main(as: [n]i32, i: i32, j: i32): []i32 =
  as[i:j]
