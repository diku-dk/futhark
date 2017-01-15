-- Test that functions accept only the right number of arguments.
-- ==
-- error: In call of function f.*expecting 2 argument\(s\).*but got 3 arguments

fun f(x: i32) (y: f64): f64 = f64(x) + y

fun main(): f64 = f 2 2.0 3
