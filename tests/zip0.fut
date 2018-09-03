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
let main(a: []i32) (b: []i32): ([]i32,[]i32) =
  unzip(zip a b)
