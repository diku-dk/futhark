-- The defunctionaliser once messed up local closures.

def main (n: i64) =
  let scale (x: i64) (y: i64) = (x + y) / n
  in map (scale 1) (iota n)
