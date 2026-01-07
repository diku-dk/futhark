def main (xs: { [10]i64 | \a -> For a (\i -> a[i] > 0) }) =
  map (\x -> if x > 0 then x else 0) xs
