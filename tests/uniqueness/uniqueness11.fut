-- Test that map does not introduce aliasing when the row type is a
-- basic type.
-- ==
-- input {
-- }
-- output {
--   0i64
-- }

let f (x: i64) = x

let g (x: i64) = x

let main: i64 =
  let a      = iota(10)
  let x      = map f a
  let a[1]   = 3
  let y      = map g x in
  y[0]
