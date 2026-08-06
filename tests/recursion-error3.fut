-- A lifted type parameter counts as higher-order for the recursion check, since
-- it may be instantiated with a function.
-- ==
-- error: passed unchanged

def foo '^a (g: a) (h: a -> a) (n: i64) : i64 =
  if n == 0
  then 0
  else foo (h g) h (n - 1)
