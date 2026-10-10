-- A top-level constant of higher-order type cannot contain consumption.
-- ==
-- error: Top-level constant of higher-order type

def upd (xs: *[]i32) (i: i32) : *[]i32 = xs with [0] = i

def h : i32 -> *[]i32 = upd (copy [1, 2, 3])

def main (_: i32) = (h 1, h 2)
