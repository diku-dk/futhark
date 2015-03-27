// Curry a simple function.

fun int add(int x, int y) = x + y

fun [int] main([int] a) =
  map(add(1), a)
