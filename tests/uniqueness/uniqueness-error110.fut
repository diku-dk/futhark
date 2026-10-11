-- A matched expression is bound to names, so if it is of higher-order type it
-- cannot contain consumption.
-- ==
-- error: Matched expression of higher-order type

def upd (xs: *[]i32) (i: i32) : *[]i32 = xs with [0] = i

def main (ys: *[]i32) = match upd ys case f -> (f 1, f 2)
