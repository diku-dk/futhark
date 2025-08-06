-- ==
-- input {
-- }
-- output {
--   [[2, 4, 6], [8, 10, 12], [14, 16, 18]]
-- }
-- structure {
--   /Screma 1
-- }
def main : [][]i32 =
  let a = map (+ 1) (map i32.i64 (iota (3 * 3)))
  let b = unflatten a
  in map (\(row: []i32) ->
            map (\(x: i32) : i32 -> x * 2) row)
         b
