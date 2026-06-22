-- ==
-- tags { no_webgpu }
-- error: cannot match

type t = #foo f64

def main (surf: t) =
  match surf
  case #foo -> true
