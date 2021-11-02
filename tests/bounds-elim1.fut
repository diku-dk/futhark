-- Optimise away another particularly simple case of bounds checking.
-- ==
-- structure gpu { SegMap/Assert 0 }

let main [n] (xs: [n]i32) =
  tabulate n (\i -> xs[i] + 2)
