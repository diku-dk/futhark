// Test that a variable shape annotation is actually bound.

fun [int] main([int,n] a) =
  map(+n, a)
