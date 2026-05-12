-- Distributed in-place update where slice is not final dimension.
-- ==
-- random input { 1i64 [2][12][2]i64 } auto output
-- structure gpu { SegMap/Update 0 }

def main [n] [m] (l: i64) (xsss: *[n][m][2]i64) =
  map (\xss -> copy xss with [0:10, l] = iota 10) xsss
