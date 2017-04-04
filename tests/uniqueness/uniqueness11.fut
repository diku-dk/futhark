-- Test that map does not introduce aliasing when the row type is a
-- basic type.
-- ==
-- input {
-- }
-- output {
--   0
-- }

let f (x: i32): i32 = x

let g (x: i32): i32 = x

let main(): i32 =
  let a      = copy(iota(10))
  let x      = map f a
  let a[1]   = 3
  let y      = map g x in
  y[0]
