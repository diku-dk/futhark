// Wrong, because the map function must return an array.

fun {[int]} main([int] a) = map(fn int (int x) => x + 2, a)
