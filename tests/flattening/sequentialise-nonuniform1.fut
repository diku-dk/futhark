-- The attribute only affects nonuniform nested parallelism; uniform nested
-- parallelism is still flattened as usual.
-- ==
-- random input { [10][100]i32 } auto output
-- structure gpu { SegRed 1 }

def main (xss: [][]i32) =
  #[flattening(only_inner)]
  #[flattening(sequentialise_nonuniform)]
  map (\xs -> i32.sum (map (* 2) xs)) xss
