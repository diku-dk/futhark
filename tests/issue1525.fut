def main s =
  [0, 1] |> map (\c -> (c...(c + 1)) |> map (\k -> s[k]) |> reduce (+) 0)
