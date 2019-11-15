-- A size restriction imposed by a local function parameter may not affect
-- anything free in the function.
-- ==
-- error: `n`.*scope violation

let main (ys: []i32) =
  let f (n: i32) (xs: [n]i32) = zip xs ys
  in f
