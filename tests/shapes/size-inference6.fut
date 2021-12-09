-- Permit inference of a type with non-constructive size parameters.
-- ==
-- input { 0i64 2i64 } output { empty([0]i64) [1i64,0i64] }

def r =
  let f = reverse
  let g = reverse
  in {f, g}

def main x y =
  (\p -> (p.f (iota x), p.g (iota y))) r
