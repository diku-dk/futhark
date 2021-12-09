-- A size restriction imposed by a local function parameter may not affect
-- anything free in the function.
-- ==
-- error: "n".*scope violation

def main (ys: []i32) =
  let f (n: i64) (xs: [n]i32) = zip xs ys
  in f
