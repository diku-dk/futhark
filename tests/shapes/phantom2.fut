-- Check that consumption does not get in the way.
-- ==
-- input { [1] [2] } output { 1 }
-- input { [1] [2,3] } error:

let f [n] (xs: *[n]i32) (_: [n]i32) =
  let xs[0] = 0
  in (xs, n)

let main (xs: *[]i32) (ys: []i32) = (f xs ys).2
