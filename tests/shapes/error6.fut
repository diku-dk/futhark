-- Respect sizes based on named parameters.
-- ==
-- error: "n"

def ap (f: (n: i64) -> [n]i64) (k: i64) : [k]i64 =
  f k

def main = ap (\n -> iota (n+1)) 10
