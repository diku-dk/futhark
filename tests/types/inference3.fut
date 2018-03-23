-- An inferred parameter can be put in an array.
-- ==
-- input { 2 } output { [2] }

let f x = [x]

let main (x: i32) = f x
