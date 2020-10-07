-- ==
-- input { 2i64 } output { 2i64 empty([0][2]i32) }

let empty 'a (x: i64) = (x, [] : [0]a)

let main x : (i64, [][x]i32) = empty x
