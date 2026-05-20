-- ==
-- property: prop_printer_error

entry gen_bad (_size: i64) (_seed: u64) : i32 =
  1i32

entry pp_crash (x: i32) : []u8 =
  let z = x - x
  let y = 1i32 / z
  in if y == 0i32
     then "zero"
     else "nonzero"

#[prop(gen(gen_bad),pprint(pp_crash))]
entry prop_printer_error (x: i32) : bool =
  x == 0i32
