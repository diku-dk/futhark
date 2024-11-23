-- ==
-- input { 1i64 2i64 }
-- output { [[true, true]] }

def f (n: i64) (m: i64) (b: bool) = replicate n (replicate m b)

def g = uncurry f

def main a b = map id (g (a,b) true)
