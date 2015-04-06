// Test a simple map, that might be irregular based on input data.
// --
// input {
//   [8,8,8,8]
// }
// output {
//   [[0, 1, 2, 3, 4, 5, 6, 7], [0, 1, 2, 3, 4, 5, 6, 7], [0, 1, 2, 3, 4, 5, 6, 7], [0, 1, 2, 3, 4, 5, 6, 7]]
// }
fun [[int]] main([int] a) =
  map(fn [int] (int n) => iota(n), a)
