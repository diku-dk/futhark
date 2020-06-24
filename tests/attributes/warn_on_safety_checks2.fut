-- ==
-- warning: Safety check required

let f (xs: []f32) i =
  xs[i]

let main (xs: []f32) (is: []i32) =
  #[warn(safety_checks)]
  map (f xs) is
