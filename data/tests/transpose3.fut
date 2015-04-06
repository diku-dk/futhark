// --
// input {
//
// }
// output {
//   [[[0, 8], [1, 9]], [[2, 10], [3, 11]], [[4, 12], [5, 13]], [[6, 14], [7, 15]]]
// }
fun [[[int]]] main () =
  let xss = iota(16) in
  let tmp = reshape( (2,4,2), xss ) in
  transpose(0,2, tmp)
