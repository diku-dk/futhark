-- ==
-- input { [[1i64,2i64,3i64],[4i64,5i64,6i64]] }
-- auto output

def main [n] [m] (xss: [n][m]i64) =
  map (\xs -> scan (+) 0 (map (* 2) xs)) xss