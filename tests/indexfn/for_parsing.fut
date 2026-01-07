def main (xs: [10]i64): {bool | \_ -> true} =
  For xs (\i -> let x = xs[i] in Range x (0,inf))
