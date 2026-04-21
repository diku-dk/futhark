-- ==
-- property: prop_ok


-- ==
-- property: prop_bad_output

-- ==
-- property: prop_bad_input

-- ==
-- property: prop_bad_arity1

-- ==
-- property: prop_bad_arity2

-- n==
-- property: prop_div_zero





-- GOOD PROPERTY + GOOD GENERATOR

#[prop(gen(gen_ok), size(10))]
entry prop_ok (x: i32) : bool =
  x == x

entry gen_ok (size: i64) (seed: i32) : i32 =
  i32.i64 size + seed




-- BAD shrinker OUTPUT TYPE


#[prop(gen(gen_ok), shrink(shrink_bad_output), size(10))]
entry prop_bad_output (x: i32) : bool =
  x != x

entry shrink_bad_output (x: i32) (y: i32) : i64 =
    39



-- BAD shrinker INPUT TYPE


#[prop(gen(gen_ok), shrink(shrink_bad_input), size(10))]
entry prop_bad_input (x: i32) : bool =
  x != x

entry shrink_bad_input (x: i32) (y: i64) : i32 =
    39  



-- BAD SHRINKER ARITY

#[prop(gen(gen_ok), shrink(shrink_bad_arity), size(10))]
entry prop_bad_arity1 (x: i32) : bool =
  x != x

entry shrink_bad_arity (x: i32) (y: i32) (z: i32) : i32 =
    39  

#[prop(gen(gen_ok), shrink(shrink_bad_arity2), size(10))]
entry prop_bad_arity2 (x: i32) : bool =
  x != x

entry shrink_bad_arity2 : i32 =
    39  



-- SHRINKER crash
-- shrink divides by zero, causing crash.
#[nprop(gen(gen_ok), shrink(shrink_div_zero), size(10))]
entry prop_div_zero (x: i32) : bool =
  x != x

entry shrink_div_zero (x: i32) (y: i32) : i32 =
    x/0

