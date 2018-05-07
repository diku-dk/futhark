-- ==
-- input { 0 32 empty(i32) }
-- output { empty([]i32) }
-- input { 32 0 empty(i32) }
-- output { empty([]i32) }

let main (n: i32) (m: i32) (xs: []i32) = transpose (unflatten n m xs)
