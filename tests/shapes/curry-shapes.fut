-- Test that shape declarations are taken into account even when the
-- function is curried.
--
-- At the time this test was written, the only way to determine the
-- success of this is to inspect the result of internalisation by the
-- compiler.
-- ==
-- input {
--   [[6,5,2,1],
--    [4,5,9,-1]]
-- }
-- output {
--   [[7,6,3,2],
--    [5,6,10,0]]
-- }

fun oneToEach(r: [n]int): [n]int =
  map (+1) r

fun main(a: [][]int): [][]int =
  map oneToEach a
