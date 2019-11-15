-- A size restriction imposed by a lambda parameter may not affect
-- anything free in the lambda.
-- ==
-- error: `n`.*scope violation

let main (ys: []i32) =
  (\(n: i32) (xs: [n]i32) -> zip xs ys)
