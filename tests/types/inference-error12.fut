-- A record turns out to be missing a field.
-- ==
-- error: expected type

def f r =
  let y = r.l2
  in (r : {l1: i32})
