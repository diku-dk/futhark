-- A recursive application must pass its higher-order arguments unchanged; here
-- the function argument is a different lambda each time.
-- ==
-- tags { disable }
-- error: passed unchanged

def go (g: i64 -> i64) (n: i64) : i64 =
  if n == 0
  then g 0
  else go (\x -> g x * 2) (n - 1)
