-- ==
-- property: prop_ok


-- ==
-- property: prop_bad_output

-- ==
-- property: prop_bad_arity

-- n==
-- property: prop_div_zero

-- ==
-- property: prop_bad_arity2

-- ==
-- property: prop_bad



-- GOOD PROPERTY + GOOD GENERATOR

#[prop(gen(gen_ok), size(10))]
entry prop_ok (x: i32) : bool =
  x == x

entry gen_ok (size: i64) (seed: i32) : i32 =
  i32.i64 size + seed




-- BAD PROPERTY OUTPUT TYPE
-- Property expects i32 returns i32

#[prop(gen(gen_ok), size(10))]
entry prop_bad_output (x: i32) : i32 =
  5



-- BAD PROPERTY ARITY

#[prop(gen(gen_ok), size(10))]
entry prop_bad_arity (x: i32) (y: i32) : bool =
  x == x

#[prop(gen(gen_ok), size(10))]
entry prop_bad_arity2 : bool =
  1 == 1

#[prop(gen(gen_ok), size(10))]
#[prop(gen(gen_ok), size(10))]
entry prop_bad (x: i32) : bool =
  x == x



-- property crash
-- prop divides by zero, causing crash.
#[nprop(gen(gen_ok), size(10))]
entry prop_div_zero (x: i32) : bool =
  if 7/0 == 7
    then true
    else false

