-- If something is put in an array, it cannot later be inferred as a
-- function.
-- ==
-- error: functional

def f x = ([x], x 2)
