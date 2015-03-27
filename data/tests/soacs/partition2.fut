// Slightly more complicated test involving arrays of tuples.

fun {[{int,int}], [{int,int}], [{int,int}]} main([int] xs, [int] ys) =
  partition(<, ==, zip(xs,ys))
