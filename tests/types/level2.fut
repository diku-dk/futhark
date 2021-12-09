-- A size restriction imposed by a lambda parameter may not affect
-- anything free in the lambda.
-- ==
-- error: "n".*scope violation

def main (ys: []i32) =
  (\(n: i64) (xs: [n]i32) -> zip xs ys)
