// Simplest possible in-place operation.
// --
// input {
//   [1,2,3,4]
//   2
//   10
// }
// output {
//   [1,2,10,4]
// }
fun [int] main(*[int] a, int i, int x) =
  let a[i] = x in
  a
