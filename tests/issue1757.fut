-- ==
-- input { -5 } error: y > 0

def main (x: i32) =
  let y = x + 2
  let z = assert (y > 0) (x + 2)
  in y + z
