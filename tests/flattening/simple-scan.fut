-- ==
-- input { [[1i64,2i64,3i64],[4i64,5i64,6i64]] }
-- output { [[1i64,3i64,6i64],[4i64,9i64,15i64]] }

def main [n] [m] (xss: [n][m]i64) =
  map (\xs -> scan (+) 0 xs) xss