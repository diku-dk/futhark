-- Test that write works in its simplest uses.
-- ==
--
-- input {
--   [0]
--   [9]
--   [3]
-- }
-- output {
--   [9]
-- }
--
-- input {
--   [-1]
--   [0]
--   [5]
-- }
-- output {
--   [5]
-- }
--
-- input {
--   [0, 1]
--   [5, 6]
--   [3, 4]
-- }
-- output {
--   [5, 6]
-- }
--
-- input {
--   [0, 2, -1]
--   [9, 7, 0]
--   [3, 4, 5]
-- }
-- output {
--   [9, 4, 7]
-- }
--
-- input {
--   [4, -1]
--   [77, 0]
--   [8, -4, 9, 1, 2, 100]
-- }
-- output {
--   [8, -4, 9, 1, 77, 100]
-- }

fun [n]i32
  main([k]i32 indexes,
       [k]i32 values,
       *[n]i32 array) =
  write(indexes, values, array)
