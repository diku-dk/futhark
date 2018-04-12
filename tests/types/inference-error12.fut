-- A record turns out to be missing a field.
-- ==
-- error: unify

let f r = let y = r.l2
          in (r: {l1: i32})
