-- The array magically becomes unique!
-- ==

let f (x: []i32): []i32 = x

let main (a: *[]i32): *[]i32 = f a
