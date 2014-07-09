fun {[real],[real]} f(*[int] b_1) =
  {[1.0],[2.0]}

fun {[real], [real]} main(int n) =
  let a = copy(iota(n)) in
  let x = f(a) in
  x
