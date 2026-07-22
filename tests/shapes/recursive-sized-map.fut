-- Make sure not to confuse the 'n' in a recursive call with the 'n' of the
-- current instance.
-- ==
-- tags { disable }
-- error: Cannot apply

def f (n: i64) : [n]i64 =
  sized n (if n == 0
           then [] : [0]i64
           else let rows = map f [n - 1]
                in [n - 1] ++ rows[0])

entry main = f
