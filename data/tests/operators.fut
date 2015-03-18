// Test that sophisticated operators (such as "greater than") work.

fun {bool,bool} main(int x, int y) =
  {x > y, x >= y}
