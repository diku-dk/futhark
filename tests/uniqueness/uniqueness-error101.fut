-- Arguments are evaluated from right to left, and the value of each is held
-- while the others are evaluated.
-- ==
-- error: Using variable "xs", but this was consumed

def f (a: []i32) (b: []i32) = map2 (+) a b

def main (xs: *[]i32) = f (xs with [0] = 5) xs
