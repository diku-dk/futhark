-- Test that functions accept only the right number of arguments.
-- ==
-- error: 2 arguments

def f (x: i32) (y: f64) : f64 = f64.i32 (x) + y

def main : f64 = f 2 2.0 3
