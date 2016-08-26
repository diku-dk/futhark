-- Test that shadowing does not break alias analysis.
-- ==
-- error:

fun main(): *[]int =
  let n = 10 in
  let a = iota(n) in
  let c = let a = a in let a[0] = 42 in a
  in a -- Should be an error, because a was consumed.
