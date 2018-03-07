-- Test that functions accept only the right number of arguments.
-- ==
-- error: Attempt to apply an expression of type f64 to an argument of type i32.

let f(x: i32) (y: f64): f64 = r64(x) + y

let main(): f64 = f 2 2.0 3
