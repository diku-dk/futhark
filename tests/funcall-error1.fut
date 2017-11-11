-- Test that functions accept only the right number of arguments.
-- ==
-- error: In call of function f.*expecting 2 arguments.*but got 3 arguments

let f(x: i32) (y: f64): f64 = r64(x) + y

let main(): f64 = f 2 2.0 3
