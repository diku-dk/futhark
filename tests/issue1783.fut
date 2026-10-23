-- ==
-- error: differ in arity

type surface =
    #asphere {curvature: f32}
  | #sphere {curvature: f32}

entry sag (surf: surface) : f32 =
  match surf
  case #asphere ->
    1
  case #sphere ->
    2
