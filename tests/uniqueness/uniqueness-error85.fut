-- The inferred result of 'f' is not fresh, as its payload components
-- alias each other.
-- ==
-- error: consumed

type t [n] = #foo ([n]i32) ([n]i32) | #bar

def f [n] (xs: *[n]i32) = #foo xs xs : t [n]

def main [n] (xs: *[n]i32) : [n]i32 =
  match f xs
  case #foo a b -> let a[0] = 1 in b
  case #bar -> replicate n 0
