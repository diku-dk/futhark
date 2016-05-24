-- Checks that the results of splits are properly copied to where they
-- are supposed to go.
-- ==
-- input {
--   [[1,2,2,1], [3,4,5,4], [6,7,8,9]]
--   2
-- }
-- output {
--   [[1,2], [3,4], [6,7]]
-- }

fun [int] take(int n, [int] r) =
  let (part, _) = split( (n), r) in
  part

fun [[int]] main([[int]] rs, int n) =
  map(take(n), rs)
