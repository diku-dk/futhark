-- A map with duplicate outputs should work.
-- ==
-- structure gpu { SegMap 0 Replicate 2 }

def main (n: i64) (m: i64) =
  map (\i -> (replicate m i, replicate m i)) (iota n)
