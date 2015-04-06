// --
// input {
//   [ [1,2,3], [4,5,6]
//   , [6,7,8], [9,10,11]
//   ]
//   [1,2,3,4]
//   5
// }
// output {
//   [[7, 8, 9],
//    [16, 17, 18],
//    [24, 25, 26],
//    [33, 34, 35]]
// }
fun [int] addToRow ([int] xs, int y) =
  map(fn int (int x) => x+y, xs)

fun [[int]] main ([[int]] xss, [int] cs, int y) =
  map (fn [int] ([int] xs, int c) =>
         let y' = y * c + c in
         let zs = addToRow(xs,y') in
         zs
      , zip (xss,cs))
