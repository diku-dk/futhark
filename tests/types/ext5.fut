-- ==
-- error: Existential size "n"

def f : (i64, i64) -> ?[n][m].(b: bool) -> *[n][m]bool =
  \(n, m) (b: bool) -> replicate n (replicate m b)

def main x = map id (f x true)
