-- ==
-- random input { [10][100]i32 } auto output
-- structure gpu { SegMap 0 SegScan 0 }

def main xss =
  #[sequential] map (\xs -> scan (+) 0i32 xs) xss
