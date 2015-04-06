// --
// input {
//   [[1,2,3],[4,5,6],[7,8,9]]
// }
// output {
//   [[3, 5, 7], [7, 10, 13], [14, 18, 22]]
// }
fun [[int]] main([[int]] input) =
  let x = scan(fn [int] ([int] a, [int] b) =>
                 map(+, zip(a, b)),
               iota(3), input) in
  map(fn [int] ([int] r) =>
        map(+ (2), r),
      x)
