-- Test that a variable shape annotation may not be a non-integer.
-- ==
-- error:

fun [int] main(f64 n, [int,!n] a) =
  map(+2, a)
