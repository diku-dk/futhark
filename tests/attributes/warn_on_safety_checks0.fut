-- ==
-- warning: Safety check required

def main (xs: []f32) i =
  #[warn(safety_checks)]
  xs[i]
