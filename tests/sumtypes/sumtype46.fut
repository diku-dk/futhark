-- ==
-- error: 0 constructor arguments

type t = #foo f64

def main (surf: t) =
  match surf
  case #foo -> true
