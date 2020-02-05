-- ==
-- input { 2 } output { 2 empty([0][1]i32) }

let empty (d: i32) (x: i32) : (i32, [0][d]i32) = (x, [])

let main (x: i32): (i32, [][1]i32) = empty 1 x
