-- We must be able to infer that sizes are never negative.
-- ==
-- structure { Assert 0 }

let main [n] (xs: [n]i32) =
  assert ((n >= 0) && !(n < 0)) xs
