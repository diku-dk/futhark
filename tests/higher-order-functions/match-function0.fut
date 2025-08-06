-- We cannot return a function from a pattern match.
-- ==
-- error: returned from pattern match

def f (x: i32) : i32 = x + x
def g (x: i32) : i32 = x + 1

def main (b: bool) (n: i32) : i32 = (match b case _ -> f) n
