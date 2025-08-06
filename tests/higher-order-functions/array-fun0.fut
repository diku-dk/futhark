-- We cannot have an array literal that contains function variables.
-- ==
-- error: functional

def f (x: i32) : i32 = x + x
def g (x: i32) : i32 = x + 1
def arr = [f, g]
