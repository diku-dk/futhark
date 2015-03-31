fun [int] addRows ([int] xs, [int] ys) =
  map(+, zip (xs,ys))

fun [[int]] main ([[int]] xss, [int] ys) =
  map(fn [int] ([int] xs) => addRows(xs,ys), xss)
