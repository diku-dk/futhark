-- A recursive application must be fully saturated: here 'repeat' is applied to
-- only two of its three parameters.
-- ==
-- tags { disable }
-- error: not fully saturated

def repeat (n: i64) (f: i64 -> i64) (x: i64) : i64 =
  if n == 0
  then x
  else let g = repeat (n - 1) f
       in g (f x)
