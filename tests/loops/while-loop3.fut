-- ==
-- input { 1 } output { 11 false }

def main (x: i32) =
  loop (x, continue) = (x, true) while continue do
    (x+1, x < 10)
