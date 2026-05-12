-- ==
-- structure gpu { Manifest 1 }

def smoothen [n] (xs: [n]f32) =
  let pick i = xs[i64.min (n - 1) (i64.max 0 i)]
  in tabulate n (\i ->
                   pick (i - 2) + pick (i - 1) * 4
                   + pick i * 6
                   + pick (i + 1) * 4
                   + pick (i + 2))

def main xss =
  xss
  |> transpose
  |> map transpose
  |> map (map smoothen)
  |> map transpose
  |> transpose
