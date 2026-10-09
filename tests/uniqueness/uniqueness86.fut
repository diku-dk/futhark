-- Freshness from parametricity survives recursion: the recursive call is to an
-- instance whose result is also fresh.
-- ==
-- input { [5, 2, 3] } output { [1, 2, 3] [5, 2, 3] }

def ap 'a 'b (f: a -> b) (x: a) (n: i32) : b =
  if n == 0 then f x else ap f x (n - 1)

def main (xs: []i32) =
  let r = ap copy xs 3
  let r[0] = 1
  in (r, xs)
