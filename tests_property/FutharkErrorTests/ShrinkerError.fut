-- ==
-- property: prop_shrinker_error

entry gen_bad (_size: i64) (_seed: u64) : i32 =
  1i32

entry shrink_crash (_x: i32) (_random: u64) : i32 =
  1i32 / 0i32

#[prop(gen(gen_bad),shrink(shrink_crash))]
entry prop_shrinker_error (x: i32) : bool =
  x == 0i32
