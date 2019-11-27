-- Test that we *do* hoist a potentially unsafe (but loop-invariant)
-- expression out of a loop.
-- ==
-- input { 4 [1,2,3] } output { 6 }
-- input { 0 empty([0]i32) } output { 0 }
-- structure { /If/True/BinOp 1 }

let main [n] (a: i32) (xs: [n]i32) =
  loop acc = 0 for x in xs do acc + x*(a/n)
