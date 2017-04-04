-- Type ascription should not hide aliases.
-- ==
-- error:

let main(): i32 =
  let a = iota(10)
  let b:*[]i32 = a
  let b[0] = 1
  in a[0]
