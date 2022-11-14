-- Unification of variables with incompletely known and distinct fields.
-- ==
-- error: must be a record with fields

def sameconst '^a (_: a) (y: a) = y

def main (x: i64) =
  let elems = sameconst (\s -> s.x < 0) (\s -> s.y > 0)
  in elems {x}
