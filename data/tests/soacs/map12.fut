// Test map in which the map function consumes its argument.
fun *[[int]] main(*[[int]] a, int i, int x) =
  map(fn *[int] (*[int] r) =>
        let r[i] = x in r,
      a)
