-- Distributed in-place update where slice is not final dimension.
-- ==
-- random input { 1 [2][12][2]i32 } auto output
-- structure distributed { SegMap/Update 0 }

let main [n][m] (l: i32) (xsss: *[n][m][2]i32) =
  map (\xss -> copy xss with [0:10,l] = iota 10) xsss
