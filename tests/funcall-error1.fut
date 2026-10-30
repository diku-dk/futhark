-- Test that functions accept only the right number of arguments.
-- ==
-- error: 2 arguments

def f (x: i32) (y: f32) : f32 = f32.i32 (x) + y

def main : f32 = f 2 2.0 3
