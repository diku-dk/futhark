-- Test genred in simple cases with addition operator
-- ==
--
-- input  {
--   [0, 0, 0, 0, 0]
--   [1, 1, 1, 1, 1]
-- }
-- output {
--   [0, 5, 0, 0, 0]
-- }
--
-- input  {
--   [0, 0, 0, 0, 0]
--   [1, 1, 4, 4, 4]
-- }
-- output {
--   [0, 2, 0, 0, 12]
-- }
--
-- input  {
--   [1, 2, 3, 4, 5]
--   [1, 1, 4, 4, 4]
-- }
-- output {
--   [1, 4, 3, 4, 17]
-- }

let main [m][n] (hist : *[n]i32) (image : [m]i32) : [n]i32 =
  reduce_by_index hist (+) 0 image image
