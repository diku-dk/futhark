type wrapper = {x: i32}

-- ==
-- property: prop_wrapper

entry gen_i32 (_size: i64) (_seed: i32) : i32 =
  0i32

#[prop(gen(gen_i32))]
entry prop_wrapper (w: wrapper) : bool =
  true
