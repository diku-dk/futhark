-- Issue with a generated variable name that matched the name of a
-- function.  This program does not compute anything interesting.
-- ==

let map2 [n] 'a 'b 'x (f: a -> b -> x) (as: [n]a) (bs: [n]b): []x =
  map (\(a, b) -> f a b) (zip as bs)

let main (n: i32) =
  let on_row (row: i32) (i: i32) = replicate row i
  let a = i32.iota n
  in map (on_row a[0]) a
