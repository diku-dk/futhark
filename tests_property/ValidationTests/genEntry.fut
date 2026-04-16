def gen (_: i64) (_: i32) : i8 =
  0

-- ==
-- property: prop
#[prop(gen(gen))]
entry prop (x: i8) : bool =
  x == 0