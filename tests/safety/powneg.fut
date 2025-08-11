-- Negative integer exponent, and in a parallel context at that!
-- ==
-- input { 2 [-1] } error:

def main (b: i32) (xs: []i32) = map (b **) xs
