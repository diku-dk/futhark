-- Basic ranges.
-- ==
-- input { 0 1 5 }
-- output {
--   [0i32, 1i32, 2i32, 3i32, 4i32, 5i32]
--   [0i32, 1i32, 2i32, 3i32, 4i32]
--   empty(i32)
--   [0i32, 1i32, 2i32, 3i32, 4i32, 5i32]
--   [0i32, 1i32, 2i32, 3i32, 4i32]
--   empty(i32)
-- }
--
-- input { 1 0 5 }
-- output {
--   [1i32, 2i32, 3i32, 4i32, 5i32]
--   [1i32, 2i32, 3i32, 4i32]
--   empty(i32)
--   empty(i32)
--   empty(i32)
--   empty(i32)
-- }
--
-- input { 5 4 1 }
-- output {
--   empty(i32)
--   empty(i32)
--   [5i32, 4i32, 3i32, 2i32]
--   [5i32, 4i32, 3i32, 2i32, 1i32]
--   empty(i32)
--   [5i32, 4i32, 3i32, 2i32]
-- }
--
-- input { 5 0 0 }
-- output {
--   empty(i32)
--   empty(i32)
--   [5i32, 4i32, 3i32, 2i32, 1i32]
--   [5i32, 0i32]
--   empty(i32)
--   [5i32]
-- }
--
-- input { 0 2 5 }
-- output {
--   [0i32, 1i32, 2i32, 3i32, 4i32, 5i32]
--   [0i32, 1i32, 2i32, 3i32, 4i32]
--   empty(i32)
--   [0i32, 2i32, 4i32]
--   [0i32, 2i32, 4i32]
--   empty(i32)
-- }

let main (start: i32) (step: i32) (end: i32) =
  ([start...end],
   [start..<end],
   [start..>end],
   [start..step...end],
   [start..step..<end],
   [start..step..>end])
