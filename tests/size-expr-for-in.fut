-- ForIn pattern must transform its size expressions
-- ==
def main (n: i64) =
  loop x = 0
  for t in replicate n (iota (n + 1)) do
    x + reduce (+) 0 t
