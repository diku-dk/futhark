-- A map with duplicate outputs should work.
-- ==
-- structure gpu { SegMap 1 }

def main (n: i64) (m: i64) =
  map (\i -> (replicate m i, replicate m i)) (iota n)
