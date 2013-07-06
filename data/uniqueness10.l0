// Test that complex shadowing does not break alias analysis.

fun *[int] main() =
  let n = 10 in
  let a = iota(n) in
  let c = let {a, b} = {iota(n), a} in let a[0] = 42 in a
  in a // OK, because the outer a was never consumed.
