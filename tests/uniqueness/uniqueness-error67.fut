-- From #2849
-- ==
-- error: self-aliased

def f [n] (x: *([n]i32, [n]i32)) = x with 0[0] = 0

def h [n] (x: *[n]i32) = f (x, x)
