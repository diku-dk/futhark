-- A function may return a global array.
-- ==
-- input { true } output { [1, 2, 3] }
-- input { false } output { [4, 5, 6] }

def global : []i32 = [1, 2, 3]

def f (b: bool) : []i32 = if b then global else [4, 5, 6]

entry main (b: bool) = f b
