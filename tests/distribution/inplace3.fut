-- Good distribution of an in-place update of a slice.  Should not
-- produce a sequential Update statement.
-- ==
-- random input { [2][12]i64 } auto output
-- structure gpu { SegMap/Update 0 }

def main [n] [m] (xss: *[n][m]i64) =
  map (\xs -> copy xs with [0:10] = iota 10) xss
