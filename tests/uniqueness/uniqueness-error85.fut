-- The inferred result of 'f' is not fresh, as its payload components
-- alias each other.
-- ==
-- error: consumed

type t 'a = #foo a a | #bar

def f 'a (x: a) = #foo x x : t a

def main [n] (xs: *[n]i32) : [n]i32 =
  match f xs
  case #foo a b -> let a[0] = 1 in b
  case #bar -> replicate n 0
