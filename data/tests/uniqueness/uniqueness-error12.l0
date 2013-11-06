// Don't let curried mapees consume more than once.

fun [int] f(*[int] a, int i) =
  let a[i] = 0 in a

fun [[int]] main() =
    let n = 10 in
    let a = iota(n) in
    let b = iota(n) in
    map(f (a), b) // Bad, because a may be consumed many times.
