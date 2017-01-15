-- Don't let curried mapees consume more than once.
-- ==
-- error:

fun f(a: *[]i32, i: i32): []i32 =
  let a[i] = 0 in a

fun main(): [][]i32 =
    let n = 10
    let a = iota(n)
    let b = iota(n) in
    map (f (a)) b -- Bad, because a may be consumed many times.
