-- ==
-- input { 2 } output { [2i32, 2i32] }

let f [n] (x: i32) : [n]i32 = replicate n x

let main (x: i32) : [x]i32 = f x
