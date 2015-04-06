// --
// input {
//   [ [ [ [1,2,3], [4,5,6] ]
//     ]
//   , [ [ [6,7,8], [9,10,11] ]
//     ]
//   , [ [ [3,2,1], [4,5,6] ]
//     ]
//   , [ [ [8,7,6], [11,10,9] ]
//     ]
//   ]
//   [1,2,3,4]
//   5
// }
// output {
//   [[[[7, 8, 9],
//      [10, 11, 12]]],
//    [[[18, 19, 20],
//      [21, 22, 23]]],
//    [[[21, 20, 19],
//      [22, 23, 24]]],
//    [[[32, 31, 30],
//      [35, 34, 33]]]]
// }
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
