// While-loop with a condition that consumes something that it has allocated itself.

fun bool pointlessly_consume(int x, *[int] a) =
  x < reduce(op+, 0, a)

fun [int] main(*[int] a, int i) =
  loop (a) = while pointlessly_consume(a[i], copy(iota(i))) do
    let a[i] = a[i] + 1 in
    a in
  a
