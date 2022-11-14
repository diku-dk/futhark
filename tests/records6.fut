-- Unification of variables with incompletely known fields.

def sameconst '^a (_: a) (y: a) = y

def main (x: i64) =
  let elems = sameconst (\s -> s.x < 0) (\s -> s.x > 0)
  in elems {x}
