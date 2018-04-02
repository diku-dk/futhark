-- Check uniqueness of return type annotation properly.
-- ==
-- error: annotated type

let f (x: []i32): []i32 = x

let main (a: *[]i32): *[]i32 = f a
