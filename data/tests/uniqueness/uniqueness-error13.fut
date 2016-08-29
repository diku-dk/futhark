-- This test checks whether uniqueness is tracked properly for the map
-- SOAC.
-- ==
-- error:

fun f (x: []int): []int = x

fun g (x: []int): []int = x

fun main(): int =
  let a      = copy(replicate(10, iota(10)))
  let x      = map f a
  let a[1]   = iota(10)
  let y      = map g x               in
  y[0]
