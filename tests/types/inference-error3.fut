-- If something is applied, it cannot later be put in an array.
-- ==
-- error: -> b

def f x = (x 2, [x])
