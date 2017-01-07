-- Test that the subtype property works for function return types.
-- ==
-- error:

fun f(a: *[]int): []int = a -- OK, because unique is a subtype of nonunique

fun g(a: []int): *[]int = a -- Wrong!

fun main(): int = 0
