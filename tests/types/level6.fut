-- A size restriction imposed by a local size parameter may not affect
-- anything free in the function.
-- ==
-- error: `n`.*scope violation

let main (ys: []i32) =
  let f [n] (xs: [n]i32) = zip xs ys
  in f
