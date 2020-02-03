-- Good distribution of an in-place update of a slice.  Should not
-- produce a sequential Update statement.
-- ==
-- random input { [2][12]i32 } auto output
-- structure distributed { SegMap/Update 0 }

let main [n][m] (xss: *[n][m]i32) =
  map (\xs -> copy xs with [0:10] = iota 10) xss
