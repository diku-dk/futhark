-- ==
-- error: Cannot generalise

def foo n =
  let (m,_) = (n+1,true)
  in (iota ((m+1)+1),
      zip (iota (m+1)),
      zip (iota m))

def main n =
  let (xs, _, _) = foo n
  in xs
