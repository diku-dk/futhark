-- Simplifying out rotate-rotate chains.
-- ==
-- input { 1 -1 [1,2,3] }
-- output { [1,2,3] }
-- input { 1 -2 [1,2,3] }
-- output { [3,1,2] }
-- structure { Rotate 1 }

let main (x: i32) (y: i32) (as: []i32) = rotate x (rotate y as)
