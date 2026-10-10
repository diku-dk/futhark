-- A sum with separate payload components can be consumed and returned
-- fresh.
-- ==
-- input { [5,6] } output { 2i64 }

type t [n] = #foo ([n]i32) ([n]i32) | #bar

def g [n] (s: *t [n]) : *t [n] = s

def h [n] (s: *t [n]) : i64 =
  match g s
  case #foo a _ -> length a
  case #bar -> 0

def main [n] (xs: [n]i32) = h (#foo (copy xs) (copy xs))
