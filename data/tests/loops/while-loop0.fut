// Simplest test of while loops.
// --
// input {
//   1
//   9
// }
// output {
//   16
// }

fun int main(int x, int bound) =
  loop (x) = while x < bound do x * 2
  in x
