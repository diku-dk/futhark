-- A record turns out to be missing a field.
-- ==
-- error: unify record type

def f r =
  let y = r.l2
  in (r : {l1: i32})
