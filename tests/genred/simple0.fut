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
--   [0, 2, 0, 3, 0]
-- }
--
-- input  {
--   [1, 2, 3, 4, 5]
--   [1, 1, 4, 4, 4]
-- }
-- output {
--   [1, 4, 3, 7, 5]
-- }

let main [m][n] (hist : *[n]i32, image : [m]i32) : [n]i32 =
  genred hist (+) 0 (\x -> (x, x)) image