entry blockify [n] (b: i64) (xs: [n][n]i32)
 =
  xs
  |> unflatten (n / b) b
  |> map transpose
  |> map (unflatten (n / b) b)
  |> map (map transpose)
