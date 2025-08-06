-- When returning unique values from a loop, they must not alias each other.
-- ==
-- error: aliases other consumed loop parameter

def main (n: i64) =
  loop {xs = xs: *[]i32, ys = ys: *[]i32} = {xs = replicate n 0, ys = replicate n 0}
  for i < 10 do
    {xs = xs, ys = xs}
