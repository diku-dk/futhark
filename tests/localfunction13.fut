-- A local function that is never used should not cause any trouble.
-- ==

entry main (x: i32) =
  let toVec (a, b, c) = [a, b, c]
  in x
