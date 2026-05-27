-- ==
-- property: prop_generator_error

entry gen_crash (_size: i64) (_seed: u64) : i32 =
  1i32 / 0i32

#[prop(gen(gen_crash))]
entry prop_generator_error (x: i32) : bool =
  x == x
