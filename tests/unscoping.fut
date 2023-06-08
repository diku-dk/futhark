-- ==
-- error: Cannot apply "bar" to "xs"

def foo n =
  let m = n+1
  in (iota ((m+1)+1),
      \_ -> iota (m+1),
      \_ -> iota m)

def bar [n] (_:[n+1]i64) = n

def main n =
  let (xs, _, _) = foo n
  in bar xs
