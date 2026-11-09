-- ==
-- random input { [10][100]i32 } auto output
-- structure gpu { Replicate 0 /Loop/SegMap 1 }

def main xss =
  #[sequential_outer] map (\xs -> map (+ 100) xs) xss
