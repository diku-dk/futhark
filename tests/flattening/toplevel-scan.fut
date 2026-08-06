-- ==
-- compiled random input { [256]i32 } auto output
-- structure gpu { /SegScan 1 }

entry main [n] (xs: [n]i32) =
  scan (+) 0 xs
