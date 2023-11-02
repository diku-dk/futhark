-- Parallelise even a complicated screma with horisontally fused scan
-- and reduce.
-- ==
-- structure gpu { /SegMap/SegScan 1 /SegMap/SegRed 1 }

entry main [n] [m] (a: [m][n]f32) =
  #[incremental_flattening(only_intra)]
  map (\ row ->
         let row_scanned = scan (+) 0 row
         in (reduce (+) 0 row, row_scanned)
      ) a
  |> unzip
