-- We cannot return a function from a conditional.
-- ==
-- error: returned from branch

def f (x: i32) : i32 = x + x
def g (x: i32) : i32 = x + 1

def main (b: bool) (n: i32) : i32 = (if b then f else g) n
