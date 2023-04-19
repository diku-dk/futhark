def main (n: i64) =
  loop (xs: []i64) = iota n for i < n do (xs ++ xs)
