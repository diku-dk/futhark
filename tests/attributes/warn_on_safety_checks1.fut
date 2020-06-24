-- ==
-- warning: Safety check required

let main (xs: []f32) (is: []i32) =
  #[warn(safety_checks)]
  map (\i -> xs[i]) is
