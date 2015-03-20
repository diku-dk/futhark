// An existing variable can be used as a shape declaration.

fun [[int,!k],!n] main(int n, int m, int k) =
  let a = replicate(n, iota(m)) in
  map(fn [int,!k] ([int,!m] r) =>
        let x = reduce(+, 0, r)
        in map(+x, iota(k)),
      a)
