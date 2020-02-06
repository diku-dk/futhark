-- Test basic constant size annotations.
-- ==
-- input {
--   [5,7,2,4,1,0,9]
-- }
-- output {
--   [5,7,2]
--   [4,1,0,9]
-- }
let main(a: [7]i32) =
  split (3) a :> ([3]i32, [4]i32)
