-- A local binding may not affect the type of an outer parameter.
-- ==
-- error: `n`.*scope violation

let main (xs: []i32) =
  let n = 2+3
  in zip (iota n) xs
