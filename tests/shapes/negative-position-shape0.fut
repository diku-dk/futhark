-- It should be allowed to have a shape parameter that is only used in
-- negative position in the parameter types.
-- ==
-- input {} output { 3 }

let f [n] (_g: i32 -> [n]i32) : i32 = n

let main = f (replicate 3)
