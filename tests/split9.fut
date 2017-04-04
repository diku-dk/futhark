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

let take (n: i32) (r: []i32): []i32 =
  let (part, _) = split (n) r in
  part

let main(rs: [][]i32, n: i32): [][]i32 =
  map (take(n)) rs
