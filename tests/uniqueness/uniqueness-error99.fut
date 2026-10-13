-- The first component of a tuple is held while the second is evaluated, so
-- the second cannot consume it.
-- ==
-- error: Using variable "xs", but this was consumed

def main (xs: *[]i32) = (xs, let _ = xs with [0] = 5 in 0i32)
