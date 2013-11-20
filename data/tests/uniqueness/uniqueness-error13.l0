// This test checks whether uniqueness is tracked properly for the map
// SOAC.

fun [int] f ([int] x) = x

fun [int] g ([int] x) = x

fun int main() =
  let a      = replicate(10, iota(10)) in
  let x      = map(f, a)               in
  let a[1]   = iota(10)                in
  let y      = map(g, x)               in
  y[0]
