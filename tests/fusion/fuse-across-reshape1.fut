-- ==
-- input {
-- }
-- output {
--   [[2, 4, 6], [8, 10, 12], [14, 16, 18]]
-- }
-- structure {
--   /Screma 1
-- }
let main: [][]i32 =
  let n = 9
  let a = map (+1) (iota(n))
  let b = unflatten 3 3 a in
  map  (\(row: []i32) ->
         map  (\(x: i32): i32 -> x*2) row) b
