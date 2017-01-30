-- Test that a variable shape annotation in a binding position may not
-- be the same as another parameter.
-- ==
-- error:

fun main(n: f64, a: [n]i32): []i32 =
  map (+2) a
