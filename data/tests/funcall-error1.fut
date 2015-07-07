-- Test that functions accept only the right number of arguments.
-- ==
-- error: In call of function f.*expecting 2 argument\(s\).*but got 3 arguments

fun real f(int x, real y) = toFloat(x) + y

fun real main() = f(2, 2.0, 3)
