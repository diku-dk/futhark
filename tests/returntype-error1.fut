-- Test that the subtype property works for function return types.
-- ==
-- error:

let f(a: *[]i32): []i32 = a -- OK, because unique is a subtype of nonunique

let g(a: []i32): *[]i32 = a -- Wrong!

let main(): i32 = 0
