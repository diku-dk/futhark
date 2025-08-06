-- Records can be used like tuples.
-- ==
-- input { 2 } output { 3 1 }

def f (x: i32) = {0 = x + 1, 1 = x - 1}

def main (x: i32) = f x
