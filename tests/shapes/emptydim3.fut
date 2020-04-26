-- ==
-- input { 2 } output { 2 empty([0][2]i32) }

let empty 'a (x: i32) = (x, [] : [0]a)

let main x : (i32, [][x]i32) = empty x
