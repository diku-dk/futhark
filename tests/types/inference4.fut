-- An inferred parameter can be returned from a branch.
-- ==
-- input { 2 } output { 2 }

let f x = if true then x else x

let main (x: i32) = f x
