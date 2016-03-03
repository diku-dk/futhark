-- Test that functions accept only the right number of arguments.
-- ==
-- error: In call of function f.*expecting 2 argument\(s\).*but got 3 arguments

fun f64 f(int x, f64 y) = f64(x) + y

fun f64 main() = f(2, 2.0, 3)
