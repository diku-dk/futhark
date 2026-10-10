-- When returning unique values from a loop, they must not alias each other.
-- ==
-- error: aliases another returned value.*aliases-previously-returned

def main (n: i64) =
  loop (xs: *[]i32, ys: *[]i32) = (replicate n 0, replicate n 0)
  for i < 10 do
    (xs, xs)
