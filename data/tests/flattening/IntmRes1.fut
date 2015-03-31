fun [int] addToRow ([int] xs, int y) =
  map(fn int (int x) => x+y, xs)

fun [[int]] main ([[int]] xss, [int] cs, int y) =
  map (fn [int] ([int] xs, int c) =>
         let y' = y * c + c in
         let zs = addToRow(xs,y') in
         zs
      , zip (xss,cs))
