-- ==
-- error: differ in arity

type t = #foo f64

def main (surf: t) =
  match surf
  case #foo x y -> x
