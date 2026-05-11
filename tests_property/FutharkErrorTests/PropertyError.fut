-- ==
-- property: prop_property_error

entry gen_i32 (_size: i64) (_seed: i32) : i32 =
  0i32

#[prop(gen(gen_i32))]
entry prop_property_error (x: i32) : bool =
  let y = 1i32 / x
  in y > 0i32


-- ==
-- property: prop_head_is_zero

type~ arr = []i32

entry gen_maybe_empty (size: i64) (_seed: i32) : arr =
  let n = if size < 0 then 0 else size
  in replicate n 0i32

#[prop(gen(gen_maybe_empty), size(0))]
entry prop_head_is_zero (xs: arr) : bool =
  xs[0] == 0i32