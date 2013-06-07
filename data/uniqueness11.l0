// Test that map does not introduce aliasing when the row type is a
// basic type.

fun int f (int x) = x

fun int g (int x) = x

fun int main() =
  let a      = iota(10)  in
  let x      = map(f, a) in
  let a[1]   = 3         in
  let y      = map(g, x) in
  y[0]
