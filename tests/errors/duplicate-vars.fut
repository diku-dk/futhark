-- Using the same name twice in a single pattern is forbidden.
--
-- ==
-- error: also bound

def main (x: i32) : (i32, i32) =
  let (y, y) = (x - 1, x + 1)
  in (y, y)
