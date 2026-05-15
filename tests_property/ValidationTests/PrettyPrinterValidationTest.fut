-- ==
-- property: prop_pp_not_entry

entry gen_i32 (size: i64) (seed: i32) : i32 =
  seed + i32.i64 size

def pp_not_entry (x: i32) : []u8 =
  "not an entry point"

#[prop(gen(gen_i32),pprint(pp_not_entry))]
entry prop_pp_not_entry (x: i32) : bool =
  x == x

-- ==
-- property: prop_pp_wrong_input

entry pp_wrong_input (xs: []i32) : []u8 =
  "wrong input type"

#[prop(gen(gen_i32),pprint(pp_wrong_input))]
entry prop_pp_wrong_input (x: i32) : bool =
  x == x

-- ==
-- property: prop_pp_wrong_output

entry pp_wrong_output (x: i32) : i32 =
  x

#[prop(gen(gen_i32),pprint(pp_wrong_output))]
entry prop_pp_wrong_output (x: i32) : bool =
  x == x

-- ==
-- property: prop_pp_wrong_arity

entry pp_wrong_arity (x: i32) (y: i32) : []u8 =
  "wrong arity"

#[prop(gen(gen_i32),pprint(pp_wrong_arity))]
entry prop_pp_wrong_arity (x: i32) : bool =
  x == x
