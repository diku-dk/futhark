-- Test that error states are not sticky from one call of an entry
-- point to the next.

let main (xs: []f32) (is: []i32) =
  map (\i -> xs[i]) is
