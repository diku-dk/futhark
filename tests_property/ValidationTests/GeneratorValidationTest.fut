-- ==
-- property: prop_ok

-- ==
-- property: prop_missing_gen

-- ==
-- property: prop_nonentry_gen

-- ==
-- property: prop_bad_output

-- ==
-- property: prop_bad_arity

-- ==
-- property: prop_bad_input_types

-- n==
-- property: prop_div_zero

-- ==
-- property: prop_tuple_ok


-- GOOD PROPERTY + GOOD GENERATOR

#[prop(gen(gen_ok), size(10))]
entry prop_ok (x: i32) : bool =
  x == x

entry gen_ok (size: i64) (seed: i32) : i32 =
  i32.i64 size + seed


-- MISSING GENERATOR
-- Should fail validation with "No generator specified ..."


#[prop(size(10))]
entry prop_missing_gen (x: i32) : bool =
  x == x


-- GENERATOR NAME EXISTS IN SOURCE BUT IS NOT AN ENTRY POINT
-- Should fail validation with "Generator is not a server entry point ..."

let helper_not_entry (size: i64) (seed: i32) : i32 =
  i32.i64 size + seed

#[prop(gen(helper_not_entry), size(10))]
entry prop_nonentry_gen (x: i32) : bool =
  x == x


-- BAD GENERATOR OUTPUT TYPE
-- Property expects i32, generator returns bool
-- Should fail validateGenTypes

#[prop(gen(gen_bad_output), size(10))]
entry prop_bad_output (x: i32) : bool =
  x == x

entry gen_bad_output (size: i64) (seed: i32) : bool =
  size > 0


-- BAD GENERATOR ARITY
-- Current implementation does NOT catch this in validateGenTypes;
-- it is caught later by sendGenInputs at execution time.

#[prop(gen(gen_bad_arity), size(10))]
entry prop_bad_arity (x: i32) : bool =
  x == x

entry gen_bad_arity (seed: i32) : i32 =
  seed


-- BAD GENERATOR INPUT TYPES
-- Current implementation does NOT catch this in validateGenTypes;
-- it is caught later by sendGenInputs at execution time.

#[prop(gen(gen_bad_input_types), size(10))]
entry prop_bad_input_types (x: i32) : bool =
  x == x

entry gen_bad_input_types (size: i32) (seed: i64) : i32 =
  size + i32.i64 seed


-- Generator crash
-- Generator divides by zero, causing crash.

#[nprop(gen(gen_div_zero), size(10))]
entry prop_div_zero (x: i32) : bool =
  x == x

entry gen_div_zero (size: i64) (seed: i32) : i32 =
  10/0


-- VALID COMPOSITE / TUPLE MATCH
-- Property input is a tuple; generator returns matching tuple output.
-- This should be accepted.

#[prop(gen(gen_tuple_ok), size(10))]
entry prop_tuple_ok (p: (i32, i32)) : bool =
  let (x, y) = p
  in x == x && y == y

entry gen_tuple_ok (size: i64) (seed: i32) : (i32, i32) =
  (i32.i64 size, seed)