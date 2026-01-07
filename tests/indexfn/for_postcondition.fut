def main (xs: [10]i64) : { [10]i64 | \ys -> For ys (\i -> Range ys (0,xs[i])) } =
  map (\x -> if x >= 0 then x else 0) xs
