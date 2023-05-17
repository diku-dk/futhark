entry blockify [n] (b: i64) (xs: [n][n]i32)
 =
  (xs :> [(n/b)*b][(n/b)*b]i32)
  |> unflatten
  |> map transpose
  |> map unflatten
  |> map (map transpose)
