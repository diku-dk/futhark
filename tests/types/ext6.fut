-- ==
-- error: used as size

def f (n: i64) (m: i64) (b: bool) = replicate n (replicate m b)

def g = uncurry f

def main x = map id (g x true)
