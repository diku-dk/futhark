-- ==
-- input {
--   [1,2,3,4]
--   [5,6,7,8]
-- }
-- output {
--   [6,8,10,12]
-- }
let main(xs: []i32, ys: []i32): []i32 =
  map (+) xs ys
