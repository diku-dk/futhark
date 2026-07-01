-- input { [[1.0f32 2.0f32 3.0f32] [4.0f32 5.0f32 6.0f32] [7.0f32 8.0f32 9.0f32]] }
-- auto output

entry main [n] [m] (a: [m][n]f32) =
  map (\row ->
         let row_scanned = scan (+) 0 row
         in (reduce (+) 0 row, row_scanned))
      a
  |> unzip
