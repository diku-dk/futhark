fun [int] addToRow ([int] xs, int y) =
  map(fn int (int x) => x+y, xs)

fun [[[int]]] main ([[[int]]] xsss, [int] cs, int y) =
  map (fn [[int]] ([[int]] xss, int c) =>
         let y' = y * c + c in
         map(fn [int] ([int] xs) =>
               addToRow(xs,y')
            , xss)
      , zip (xsss,cs))
