// --
// input {
//   [1,2,3,4]
// }
// output {
//   [0, 2, 9, 24]
// }
fun [int] main ([int] xs) =
  map( fn int (int x) =>
         let tmp1 = iota(x) in
//         let tmp2 = map(*x,tmp1) in
         reduce(+,0,tmp1)
     , xs)
