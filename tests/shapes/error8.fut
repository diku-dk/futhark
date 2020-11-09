-- ==
-- error: Entry point

let empty 'a (x: i32) = (x, [] : [0]a)

let main x : (i32, [][]i32) = empty x
