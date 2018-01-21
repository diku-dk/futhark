-- We cannot have an array literal that contains function variables.
-- ==
-- error: Cannot form an array with elements of type .* -> .*

let f (x:i32) : i32 = x+x
let g (x:i32) : i32 = x+1
let arr = [f, g]
