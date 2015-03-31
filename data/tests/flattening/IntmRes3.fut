fun [int] addToRow ([int] xs, int y) =
  map(fn int (int x) => x+y, xs)

fun [[[[int]]]] main ([[[[int]]]] xssss, [int] cs, int y) =
  map (fn [[[int]]] ([[[int]]] xsss, int c) =>
         let y' = y * c + c in
         map (fn [[int]] ([[int]] xss) =>
                map(fn [int] ([int] xs) =>
                      addToRow(xs,y')
                   , xss)
            , xsss)
      , zip (xssss,cs))
