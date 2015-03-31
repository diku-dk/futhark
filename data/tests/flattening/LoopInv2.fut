fun [int] addRows ([int] xs, [int] ys) =
  map(+, zip (xs,ys))

fun [[[int]]] main ([[[int]]] xsss, [int] ys) =
  map (fn [[int]] ([[int]] xss) =>
         map(fn [int] ([int] xs) => addRows(xs,ys), xss)
      , xsss)
