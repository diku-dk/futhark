// Test that filter can be fused into reduce.
// --
// input {
//   [9,-3,5,2]
// }
// output {
//   6
// }

fun bool divisibleBy(int x, int y) = y % x == 0

fun int main([int] a) =
  let threes = filter(divisibleBy(3), a) in
  reduce(+, 0, threes)
