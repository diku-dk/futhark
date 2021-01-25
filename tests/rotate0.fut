-- Simplifying out rotate-rotate chains.
-- ==
-- input { 1i64 -1i64 [1,2,3] }
-- output { [1,2,3] }
-- input { 1i64 -2i64 [1,2,3] }
-- output { [3,1,2] }
-- input { 7i64 -5i64 [1, 2, 3] }
-- output { [3,1,2] }
-- input { -2i64 3i64 empty([0]i32) }
-- output { empty([0]i32) }
-- structure { Rotate 1 }

let main (x: i64) (y: i64) (as: []i32) = rotate x (rotate y as)
