-- Don't let curried mapees consume more than once.
-- ==
-- error:

fun [int] f(*[int] a, int i) =
  let a[i] = 0 in a

fun [[int]] main() =
    let n = 10 in
    let a = copy(iota(n)) in
    let b = copy(iota(n)) in
    map(f (a), b) -- Bad, because a may be consumed many times.
