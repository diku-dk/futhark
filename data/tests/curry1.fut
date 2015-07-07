-- Test that we can curry even "complex" arguments.
-- ==
-- input {
--   [1.0,6.0,3.0,4.0,1.0,0.0]
-- }
-- output {
--   267.000000
-- }

fun real f({int, real} x, real y) =
    let {a,b} = x in y*toFloat(a)+b

fun real g([{real, real}] x, real y) =
    let {a,b} = unzip(x) in
    reduce(+, y, a) + reduce(+, y, b)

fun real main([real] a) =
  let b = map(f ({5,6.0}), a) in
  let c = map(g (zip([1.0,2.0,3.0], [4.0,5.0,6.0])), a) in
  reduce(+, 0.0, b) + reduce(+, 0.0, c)
