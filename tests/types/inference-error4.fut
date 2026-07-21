-- If something is used in a loop, it cannot later be inferred as a
-- function.
-- ==
-- error: function type

def f x = (loop x = x for i < 10 do x, x 2)
