-- The array magically becomes unique!
-- ==

def f 't (x: []t) : []t = x

def main (a: *[]i32) : *[]i32 = f a
