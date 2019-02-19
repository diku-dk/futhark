-- ==
-- input { 2 } output { 2 empty([]i32) }

let empty (x: i32) = (x, [])

let main (x: i32): (i32, [][]i32) = empty x
