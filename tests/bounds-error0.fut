-- Test that a trivial runtime out-of-bounds access is caught.
-- ==
-- input { [1,2,3] 4 }
-- error: Assertion.*failed

fun main(a: []i32, i: i32): i32 =
  a[i]
