// --
// input {
//   [1,2,3]
// }
// output {
//   [2, 3, 4]
// }
fun [{int}] main([int] a) = map(fn {int} (int x) => {x+1}, a)
