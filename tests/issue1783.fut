-- ==
-- error: cannot match

type surface =
    #asphere {curvature: f64}
  | #sphere {curvature: f64}

entry sag (surf: surface) : f64 =
  match surf
  case #asphere ->
    1
  case #sphere ->
    2
