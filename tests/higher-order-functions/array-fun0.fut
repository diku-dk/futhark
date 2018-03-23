-- We cannot have an array literal that contains function variables.
-- ==
-- error: functional

let f (x:i32) : i32 = x+x
let g (x:i32) : i32 = x+1
let arr = [f, g]
