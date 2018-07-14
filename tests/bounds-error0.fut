-- Test that a trivial runtime out-of-bounds access is caught.
-- ==
-- input { [1,2,3] 4 }
-- error: Index \[4\] out of bounds

let main(a: []i32, i: i32): i32 =
  a[i]
