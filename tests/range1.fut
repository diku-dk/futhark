-- Range exclusion
-- ==
-- input { 10 20 }
-- output {
--   empty([0]i32)
--   empty([0]i32)
--   empty([0]i32)
-- }
-- input { 1 2 }
-- output {
--   empty([0]i32)
--   empty([0]i32)
--   empty([0]i32)
-- }
-- input { 20 10 }
-- output {
--   empty([0]i32)
--   empty([0]i32)
--   empty([0]i32)
-- }
-- input { 20 -10 }
-- output {
--   empty([0]i32)
--   empty([0]i32)
--   empty([0]i32)
-- }
-- input { 5 0 }
-- output {
--   empty([0]i32)
--   empty([0]i32)
--   empty([0]i32)
-- }
-- input { 5 -1 }
-- output {
--   empty([0]i32)
--   empty([0]i32)
--   empty([0]i32)
-- }

let main(start: i32)(end: i32) =
    (start..start...end,
    start..start..<end,
    start..start..>end)
