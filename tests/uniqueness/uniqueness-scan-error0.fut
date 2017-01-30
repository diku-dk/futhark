-- If the reduction function accumulator type is unique, consume the
-- initial value.
-- ==
-- error: .*consumed.*

fun main(a: *[]i32): []i32 =
  let b = scan (\(acc: *[]i32) (i: *[]i32): *[]i32  -> acc) a (replicate 10 (iota(10))) in
  [a[0], b[0,0]] -- Should fail, because a has been consumed!
