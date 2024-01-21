-- Test that manifest is not optimised away.
-- ==
-- input { [[1,2,3], [4,5,6], [7,8,9]] } output { 8 }
-- structure { Manifest 1 }

entry main (xs: [][]i32) =
  let ys = manifest (transpose xs)
  in ys[1,2]
