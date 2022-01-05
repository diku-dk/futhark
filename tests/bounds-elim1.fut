-- Optimise away another particularly simple case of bounds checking.
-- ==
-- structure gpu { SegMap/Assert 0 }

def main [n] (xs: [n]i32) =
  tabulate n (\i -> xs[i] + 2)
