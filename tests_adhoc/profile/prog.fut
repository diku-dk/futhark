-- ==
-- entry: inc1 inc2
-- input { [1,2,3,4] }

entry inc1 (xs: []i32) = map (+1) xs
entry inc2 (xs: []i32) = map (+2) xs
