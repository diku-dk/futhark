-- Test that the subtype property works for function return types.
-- ==
-- error:

fun [int] f(*[int] a) = a -- OK, because unique is a subtype of nonunique

fun *[int] g([int] a) = a -- Wrong!

fun int main() = 0
