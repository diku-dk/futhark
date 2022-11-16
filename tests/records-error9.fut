-- Unification of incomplete record variable with non-record.
-- ==
-- error: Cannot unify a record type with a non-record type

def sameconst '^a (_: a) (y: a) = y

def main (x: i64) =
  let elems = sameconst (\s -> s.x < 0) (\s -> s > 0)
  in elems {x}
