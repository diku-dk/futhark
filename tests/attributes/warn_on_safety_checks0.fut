-- ==
-- warning: Safety check required

let main (xs: []f32) i =
  #[warn(safety_checks)]
  xs[i]
