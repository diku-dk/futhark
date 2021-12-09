-- ==
-- input { [1i64,2i64,3i64] } output { 4i64 }

def apply 'a (f: a -> a) (x: a) = f x

def f [n] (xs: [n]i64) (x: i64) = n + x

def main (xs: []i64) = apply (f xs) 1
