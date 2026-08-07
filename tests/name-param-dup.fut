-- It is acceptable (but perhaps poor style) for a function and a parameter to
-- have the same name, and in that case the parameter takes precedence.
-- ==
-- input { 10 } output { 12 }

def f (f: i32) = f + 2

entry main = f
