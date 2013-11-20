// Test that we can curry even "complex" arguments.

fun real f({int, real} x, real y) =
    let {a,b} = x in y*toReal(a)+b

fun real g([{real, real}] x, real y) =
    let {a,b} = unzip(x) in
    reduce(op +, y, a) + reduce(op +, y, b)

fun real main([real] a) =
  let b = map(f ({5,6.0}), a) in
  let c = map(g (zip([1.0,2.0,3.0], [4.0,5.0,6.0])), a) in
  reduce(op +, 0.0, b) + reduce(op +, 0.0, c)
