-- Like phantom0.fut, but with a constant result.
-- ==
-- input { [1] [2] } output { 0 }
-- input { [1] [2,3] } error:

let f [n] (_: [n]i32) (_: [n]i32) = 0i32

let main (xs: []i32) (ys: []i32) = f xs ys
