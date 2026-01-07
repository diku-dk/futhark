def main (xs: [10]i64): {bool | \_ -> true} =
  For xs (\i -> Range xs (0,inf))
