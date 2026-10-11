-- ==
-- error: result of applying "f".*consumed

def f 'a (x: a) : (a, a) = (x, x)

def main n =
  let (a, b) = f (iota n)
  let a[0] = 0
  in (a, b)
