-- It is an error to impose two different names on the same dimension
-- in a function parameter.
--
-- ==
-- error: do not match

def f [n] [m] ((_, elems: [n]i64): (i64, [m]i64)) : i64 =
  n + m + elems[0]

def main (x: i64, y: []i64) : i64 = f (x, y)
