// An existing variable can be used as a shape declaration.

fun [[int,!k],!n] main(int n, int m, int k) =
  let a = replicate(n, iota(m)) in
  zipWith(fn [int,!k] (int i, [int,!m] r) =>
            let x = reduce(+, 0, r)
            in map(+i, map(+x, iota(k))),
          iota(n), a)
