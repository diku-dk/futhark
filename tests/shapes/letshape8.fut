-- ==
-- error: Cannot bind \[n\]

let main =
  let [n] (f: [n]bool -> [n]bool) = (\(xs: [10]bool) -> xs)
  in n
