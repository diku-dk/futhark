-- An operator section captures its operand, as a lambda captures a free
-- variable, so it cannot supply an argument for a consuming parameter.
-- ==
-- error: section cannot consume what it captures

def (<-<) (xs: *[]i32) (i: i32) : *[]i32 = xs with [0] = i

def main (ys: *[]i32) = (ys <-<) 1
