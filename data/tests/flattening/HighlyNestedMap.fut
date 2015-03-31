fun [int] add1 ([int] xs, [int] ys) =
  map(+, zip (xs,ys))

fun [[int]] add2 ([[int]] xs, [[int]] ys) =
  map (add1, zip (xs,ys))

fun [[[int]]] add3 ([[[int]]] xs, [[[int]]] ys) =
  map (add2, zip (xs,ys))

fun [[[[int]]]] add4 ([[[[int]]]] xs, [[[[int]]]] ys) =
  map (add3, zip (xs,ys))

fun [[[[int]]]] main([[[[int]]]] A, [[[[int]]]] B) =
  add4(A,B)
