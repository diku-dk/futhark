-- Test that manifest is not optimised away.
-- ==
-- structure { Manifest 1 }

entry main (xs: [][]i32) =
  let ys = manifest (transpose xs)
  in ys[1,2]
