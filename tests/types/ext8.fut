-- ==
-- input { 2i64 3i64 }
-- output { true }

def f (n: i64) (m: i64) : ([n][m](), [n][m]bool -> bool) =
  (replicate n (replicate m ()), \b -> b[0, 0])

def g = uncurry f

def main x y =
  let (a, f) = g (x, y)
  in f (map (map (const true)) a)
