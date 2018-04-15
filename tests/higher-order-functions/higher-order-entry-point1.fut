-- Curried entry point.
-- ==
-- input { 2 2 } output { 4 }

let plus (x: i32) (y: i32) = x + y

let main (x: i32) = plus x
