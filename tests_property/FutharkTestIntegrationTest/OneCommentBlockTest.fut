-- ==
-- property: prop_i32_fail

-- ==
-- property: prop_i32_fail

-- ==
-- property: prop_i32_succ

-- ==
-- property: prop_i32_succ

-- ==
-- property: prop_i32_fail
-- property: prop_i32_succ

entry gen_zero (_size: i64) (_seed: u64) : i32 =
  0i32

entry gen_one (_size: i64) (_seed: u64) : i32 =
  1i32

#[prop(gen(gen_zero))]
entry prop_i32_succ (x: i32) : bool =
  x == 0i32

#[prop(gen(gen_one))]
entry prop_i32_fail (x: i32) : bool =
  x == 0i32
