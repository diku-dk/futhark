-- If a name is used as a size, then it's probably an i32!
-- ==
-- input { 3 [1,2,3] } output { [1,2,3] }
-- input { 2 [1,2,3] } error: 2

let main n (xs: [n]i32) = xs
