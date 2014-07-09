fun *[int] f(*[int] b_1) =
  copy(iota(10))

fun [int] main(int n) =
  let a = copy(iota(n)) in
  let x = if n == 0 then a else f(a) in
  x
