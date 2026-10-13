-- ==
-- compiled random input { [256]i32 } auto output
-- structure gpu { /SegRed 1 }

entry main [n] (xs: [n]i32) =
  reduce (+) 0 xs
