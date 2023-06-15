-- ==
-- error: Cannot generalise

def foo n =
  let m = n+1
  in (iota ((m+1)+1),
      zip (iota (m+1)),
      zip (iota m))

def main n =
  let (xs, _, _) = foo n
  in xs
