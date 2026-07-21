-- The recursion check must also inspect applications nested inside local
-- functions and lambdas.
-- ==
-- tags { disable }
-- error: passed unchanged

def repeat (n: i64) (f: i64 -> i64) (x: i64) : i64 =
  let go (y: i64) = repeat (n - 1) (\z -> f z + 1) y
  in if n == 0 then x else go (f x)
