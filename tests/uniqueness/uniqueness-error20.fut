-- Test that you can't consume a free variable in a lambda.
-- ==
-- error: unique

def main (n: i64) : i32 =
  let a = iota (n)
  let b = map (\(x: i32) : i32 -> let a[x] = 4 in a[x]) (iota (n))
  in 0
