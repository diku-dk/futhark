-- Test that consumption checking is done even with no meaningful
-- bindings.
-- ==
-- error:

fun consume(a: *[]i32): i32 = 0 -- OK.

fun main(a: *[]i32): []i32 =
  let _ = consume(a) in
  a -- Should fail, because a has been consumed!
