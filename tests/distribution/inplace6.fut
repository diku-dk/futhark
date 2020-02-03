-- Distributed in-place update where slice is final dimension but there are more indexes.
-- ==
-- random input { 1 [2][2][12]i32 } auto output
-- structure distributed { SegMap/Update 0 }

let main [n][m] (l: i32) (xsss: *[n][2][m]i32) =
  map (\xss -> copy xss with [l, 0:10] = iota 10) xsss
