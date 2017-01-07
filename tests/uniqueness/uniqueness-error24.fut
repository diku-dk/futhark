-- Test that consumption checking is done even with no meaningful
-- bindings.
-- ==
-- error:

fun consume(a: *[]int): int = 0 -- OK.

fun main(a: *[]int): []int =
  let _ = consume(a) in
  a -- Should fail, because a has been consumed!
