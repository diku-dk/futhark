-- Higher-order inference.
-- ==
-- input { 2 } output { 4 }
def apply f x = f x

def main x = apply (apply i32.(+) x) x