-- A function with constraints based on named parameters cannot be
-- passed to a higher-order function that does not obey those
-- constraints.
-- ==
-- error: do not match

def ap (f: i64 -> []i32 -> i32) (k: i32) : i32 =
  f 0 [k]

def g (n: i64) (xs: [n]i32) : i32 =
  xs[n - 1]

def main (k: i32) = ap g k
