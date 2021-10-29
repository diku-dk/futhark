-- Multiple intra-group reductions.
-- ==
-- random input { [1][1][256]i32 } auto output
-- compiled random input { [10][10][256]i32 } auto output
-- structure gpu { SegMap/SegRed 1 }

let main =
  map (\xss ->
         #[incremental_flattening(only_intra)]
         map (\xs -> (i32.sum xs, i32.product xs)) xss)
      >-> map unzip >-> unzip
