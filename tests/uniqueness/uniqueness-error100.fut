-- The array being indexed is held while the index is evaluated, so the index
-- cannot consume it.
-- ==
-- error: Using variable "xs", but this was consumed

def main (xs: *[]i32) = xs[let ys = xs with [0] = 5 in ys[1]]
