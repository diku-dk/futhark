-- ==
-- input { 1 empty([0]i32) } output { empty([1][0]i32) }
-- input { 0 [1]           } output { empty([0][1]i32) }
-- input { 0 empty([0]i32) } output { empty([0][0]i32) }

let main (n: i32) (xs: []i32) = replicate n xs
