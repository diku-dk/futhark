-- The array magically becomes unique!
-- ==

def f (x: []i32) : []i32 = x

def main (a: *[]i32) : *[]i32 = f a
