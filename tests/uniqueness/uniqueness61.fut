-- From #2849
-- ==

def f [n] (x: *([n]i32, [n]i32)) = (x with 0[0] = 0) with 1[0] = 0

def g [n] (x: *([n]i32, [n]i32)) = f x

entry main [n] (x: *[n]i32, y: *[n]i32) = g (x, y)
