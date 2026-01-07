def main (xs: [10]i64) : { [10]i64 | \res -> For res (\i -> res[i] == xs[i] + 1) } =
  map (+1) xs
