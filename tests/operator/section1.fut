-- Operator section as argument to map2 library function.
-- ==
-- input { [4,2,1] [5,6,3] } output { [9,8,4] }

let main (xs: []i32) (ys: []i32) = map2 (i32.+) xs ys
