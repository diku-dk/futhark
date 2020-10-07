-- ==
-- input { 2i64 } output { [2i64, 2i64] }

let f [n] (x: i64) : [n]i64 = replicate n x

let main (x: i64) : [x]i64 = f x
