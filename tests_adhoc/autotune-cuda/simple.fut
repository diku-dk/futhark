-- ==
-- random input { [100000][100]i32 }
let main [n][m] (xss: [n][m]i32) =
  map (\row ->
         loop row for _i < 64 do
           map (+ 1) row
           |> scan (+) 0)
      xss
