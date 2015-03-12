// Test a while loop that has an array merge variable and checks it in
// its condition.

fun [int] main([int] a, int i, int bound) =
  loop (a) = while a[i] < bound do
    map(op + (1), a) in
  a
