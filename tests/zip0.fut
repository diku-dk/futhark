-- Basic test that zip doesn't totally mess up everything.
-- ==
-- input {
--   [1,2,3]
--   [4,5,6]
-- }
-- output {
--   [1, 2, 3]
--   [4, 5, 6]
-- }
let main [n] (a: [n]i32) (b: [n]i32): ([]i32,[]i32) =
  unzip(zip a b)
