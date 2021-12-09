-- ==
-- warning: Safety check required

def main (xs: []f32) (is: []i32) =
  #[warn(safety_checks)]
  map (\i -> xs[i]) is
