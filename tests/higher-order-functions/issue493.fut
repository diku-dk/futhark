-- It should be possible for a partially applied function to refer to
-- a first-order (dynamic) function in its definition.
-- ==
-- input { 3i64 [[1,2],[3,4]] }
-- output { [[[1,2],[3,4]],[[1,2],[3,4]],[[1,2],[3,4]]] }

def apply 'a '^b (f: a -> b) (x: a) = f x

def main (n: i64) (d: [][]i32) = apply (replicate n) d
