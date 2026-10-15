-- The payload of a consumed sum can be taken apart by pattern matching
-- and its components consumed separately, as with a tuple.
-- ==
-- input { [5,6] } output { [1,6] [6,7] }

type t [n] = #foo ([n]i32) ([n]i32) | #bar

def f [n] (s: *t [n]) : ([n]i32, [n]i32) =
  match s
  case #foo a b -> let a[0] = 1 in (a, b)
  case #bar -> (replicate n 0, replicate n 0)

def main [n] (xs: [n]i32) = f (#foo (copy xs) (map (+ 1) xs))
