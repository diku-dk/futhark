// Simplest test of while loops.

fun int main(int x, int bound) =
  loop (x) = while x < bound do x * 2
  in x
