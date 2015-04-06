// --
// input {
//   [[1,2,3],[1,2,3]]
//   [[3,2,1],[6,7,8]]
// }
// output {
//   [[4,4,4],[7,9,11]]
// }
fun [int] addRows ([int] xs, [int] ys) =
  map(+, zip (xs,ys))

fun [[int]] addMatricies ([[int]] A, [[int]] B) =
  map (addRows, zip (A,B))

fun [[int]] main([[int]] A, [[int]] B) =
  addMatricies(A,B)
