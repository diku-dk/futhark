-- Ensure that zipping/unzipping does not remove aliases.
-- ==
-- error: consumed

let main [n] (xs: *[n]i32, ys: *[n]i32) =
  let arrays = zip xs ys
  let (xs', ys') = unzip arrays
  let arrays[0] = (0,0)
  in xs'
