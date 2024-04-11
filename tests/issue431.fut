-- Applying tiling inside of fused scanomaps.
-- ==
-- structure gpu { /If/True/SegScan 1 /If/False/SegScan 1 }

def main (xs: []i32) =
  scan (+) 0 (map (\x -> reduce (+) 0 (map (+x) xs)) xs)
