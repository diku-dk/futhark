// Test that a variable shape annotation in a binding position may not
// be the same as another parameter.

fun [int] main(real n, [int,n] a) =
  map(+2, a)
