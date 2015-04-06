// While-loop with a condition that consumes something that it has allocated itself.
// --
// input {
//   [5,4,2,8,1,9,9]
//   4
// }
// output {
//   [5, 4, 2, 8, 6, 9, 9]
// }

fun bool pointlessly_consume(int x, *[int] a) =
  x < reduce(+, 0, a)

fun [int] main(*[int] a, int i) =
  loop (a) = while pointlessly_consume(a[i], copy(iota(i))) do
    let a[i] = a[i] + 1 in
    a in
  a
