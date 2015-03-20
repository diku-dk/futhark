// Test that filter can be fused into reduce.

fun bool divisibleBy(int x, int y) = y % x == 0

fun int main([int] a) =
  let threes = filter(divisibleBy(3), a) in
  reduce(+, 0, threes)
