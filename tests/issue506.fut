-- Issue with a generated variable name that matched the name of a
-- function.  This program does not compute anything interesting.
-- ==

def map2 [n] 'a 'b 'x (f: a -> b -> x) (as: [n]a) (bs: [n]b) : []x =
  map (\(a, b) -> f a b) (zip as bs)

def main (n: i64) =
  let on_row (row: i64) (i: i64) = replicate row i
  let a = iota n
  in map (on_row a[0]) a
