-- If something is used for equality, it cannot later be inferred as a
-- function.
-- ==
-- error: equality

def f x = (x == x, x 2)
