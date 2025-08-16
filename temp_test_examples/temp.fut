
def main [n][m] (xss: [n][m]i32) =
  scan (map2 (+)) (replicate m 0) (map (scan (+) 0) xss) |> map (reduce (+) 0)
