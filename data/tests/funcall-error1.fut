// Test that functions accept only the right number of arguments.

fun real f(int x, real y) = toReal(x) + y

fun real main() = f(2, 2.0, 3)
