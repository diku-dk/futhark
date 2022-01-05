-- ==
-- warning: Safety check required

def f (xs: []f32) i =
  xs[i]

def main (xs: []f32) (is: []i32) =
  #[warn(safety_checks)]
  map (f xs) is
