-- Range exclusion
-- ==
-- input { 10 20 }
-- output {
--   empty(i32)
--   empty(i32)
--   empty(i32)
-- }
-- input { 1 2 }
-- output {
--   empty(i32)
--   empty(i32)
--   empty(i32)
-- }
-- input { 20 10 }
-- output {
--   empty(i32)
--   empty(i32)
--   empty(i32)
-- }
-- input { 20 -10 }
-- output {
--   empty(i32)
--   empty(i32)
--   empty(i32)
-- }
-- input { 5 0 }
-- output {
--   empty(i32)
--   empty(i32)
--   empty(i32)
-- }
-- input { 5 -1 }
-- output {
--   empty(i32)
--   empty(i32)
--   empty(i32)
-- }

let main(start: i32)(end: i32) =
    ([start..start...end],
    [start..start..<end],
    [start..start..>end])
