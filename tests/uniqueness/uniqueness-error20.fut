-- Test that you can't consume a free variable in a lambda.
-- ==
-- error:

fun main(n: int): int =
  let a = iota(n)
  let b = map (\(x: int): int  -> let a[x] = 4 in a[x]) (iota(n)) in
  0
