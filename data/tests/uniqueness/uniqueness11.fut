-- Test that map does not introduce aliasing when the row type is a
-- basic type.
-- ==
-- input {
-- }
-- output {
--   0
-- }

fun f (x: int): int = x

fun g (x: int): int = x

fun main(): int =
  let a      = copy(iota(10))  in
  let x      = map f a in
  let a[1]   = 3         in
  let y      = map g x in
  y[0]
