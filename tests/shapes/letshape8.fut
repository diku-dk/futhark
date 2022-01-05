-- ==
-- error: Cannot bind \[n\]

def main =
  let [n] (f: [n]bool -> [n]bool) = (\(xs: [10]bool) -> xs)
  in n
