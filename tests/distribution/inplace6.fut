-- Distributed in-place update where slice is final dimension but there are more indexes.
-- ==
-- random input { 1i64 [2][2][12]i64 } auto output
-- structure gpu { SegMap/Update 0 }

def main [n] [m] (l: i64) (xsss: *[n][2][m]i64) =
  map (\xss -> copy xss with [l, 0:10] = iota 10) xsss
