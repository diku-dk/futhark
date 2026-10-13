-- Uniform segmented scan.
-- ==
-- input { [[1i64,2i64,3i64],[4i64,5i64,6i64]] }
-- auto output
-- structure gpu { /SegScan 1 /Apply 0 }

def main [n] [m] (xss: [n][m]i64) =
  map (\xs -> scan (+) 0 (map (* 2) xs)) xss
