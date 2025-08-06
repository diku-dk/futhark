-- This is not OK, because it would imply consuming the original
-- non-unique array.
-- ==
-- error: Unique-typed return value

def f (x: []i32) : []i32 = x

def main (a: []i32) : *[]i32 = f a
