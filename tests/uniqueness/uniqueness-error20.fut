-- Test that you can't consume a free variable in a lambda.
-- ==
-- error:

fun main(n: i32): i32 =
  let a = iota(n)
  let b = map (\(x: i32): i32  -> let a[x] = 4 in a[x]) (iota(n)) in
  0
