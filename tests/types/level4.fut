-- A size restriction imposed by a local function parameter may not affect
-- a constructor of anything free in the function.
-- ==
-- error: `n`.*scope violation

let main x =
  let f (n: i32) (xs: [n]i32) = zip xs (match x case #ys (ys: [n]i32) -> ys
                                                case _ -> xs)
  let x' = (x : (#ys ([]i32) | #null))
  in f
