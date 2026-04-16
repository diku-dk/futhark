entry gen (_: i32) (_: i32) : i8 =
  0

-- ==
-- property: prop
#[prop(gen(gen))]
entry prop (x: i8) : bool =
  x == 0