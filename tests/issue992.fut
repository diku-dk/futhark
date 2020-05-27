-- ==
-- input { [1,2,3] } output { [1,2,3] }

let main [n] (xs: [n]i32) = reverse (reverse xs)
