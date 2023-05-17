-- ==
-- input {
--   [0, 1, 2, 3, 4, 5, 6, 7, 8]
-- }
-- output {
--   [1, 2, 3, 4, 5, 6, 7, 8, 9]
--   [[2, 4, 6], [8, 10, 12], [14, 16, 18]]
-- }
def main (orig: [3*3]i32): ([]i32,[][]i32) =
  let a = map (+1) orig
  let b = unflatten a
  let c = map  (\(row: []i32) ->
                    map  (\(x: i32): i32  -> x*2) row
              ) b
  in (a,c)
