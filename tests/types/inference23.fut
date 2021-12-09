-- Inferring a function parameter into a tuple in an interesting way.
-- ==
-- input { 1 2 } output { 1 2 }

def curry f x y = f (x, y)
def id x = x

def main (x: i32) (y: i32) = curry id x y
