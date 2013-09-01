fun *[int] f(*[int] b_1) =
  iota(10)

fun [int] main(int n) =
  let a = iota(n) in
  let x = if n = 0 then a else f(a) in
  x
