-- It is OK for part of a returned tuple to be opaque.
-- ==
-- warning: ^$

type opaque [n] = [n](i32, i32)

def main (x: i32) : (opaque [], i32) = ([(x, x)], x)
