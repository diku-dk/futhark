-- Test that the subtype property works for function return types.
-- ==
-- error:

fun f(a: *[]i32): []i32 = a -- OK, because unique is a subtype of nonunique

fun g(a: []i32): *[]i32 = a -- Wrong!

fun main(): i32 = 0
