-- ==
-- error: Existential size would appear

def f (n: i64) (m: i64) (b: [n][m]bool) = b[0, 0]

def g = uncurry f

def main x y = g x y
