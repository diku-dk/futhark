fun [int] addRows ([int] xs, [int] ys) =
  map(+, zip (xs,ys))

fun [[[[int]]]] main ([[[[int]]]] xssss, [int] cs, int y) =
  map (fn [[[int]]] ([[[int]]] xsss, int c) =>
         let yss = reshape ( (2,c), xsss ) in
         map (fn [[int]] ([[int]] xss) =>
                map(fn [int] ([int] xs, [int] ys) =>
                      addRows(xs,ys)
                   , zip (xss, yss))
            , xsss)
      , zip (xssss,cs))
