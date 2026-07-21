-- A recursive function with a higher-order parameter may not be referenced
-- except as a saturated application, so it cannot be aliased and then applied
-- with a changed argument.
-- ==
-- tags { disable }
-- error: higher-order parameter

def repeat (n: i64) (f: i64 -> i64) (x: i64) : i64 =
  if n == 0
  then x
  else let g = repeat in g (n - 1) f (f x)
