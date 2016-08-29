-- Test that complex shadowing does not break alias analysis.
-- ==
-- error:

fun main(): *[]int =
  let n = 10
  let a = iota(n)
  let c = let (a, b) = (2, a) let b[0] = 42 in b
  in a -- Should be an error, because a was consumed.
