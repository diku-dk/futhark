// Wrong, because the mapT function must return an array.

fun {[int]} main([int] a) = mapT(fn int (int x) => x + 2, a)
