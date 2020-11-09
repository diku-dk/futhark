-- This is not OK, because it would imply consuming the original
-- non-unique array.
-- ==
-- error: Unique return value

let f (x: []i32): []i32 = x

let main (a: []i32): *[]i32 = f a
