-- ==
-- input {
-- }
-- output {
--   [[2, 8, 14], [4, 10, 16], [6, 12, 18]]
-- }
-- structure { /Screma 1 }
def main: [][]i32 =
  let n = 9
  let a = map (+1) (map i32.i64 (iota(n)))
  let b = unflatten 3 3 a
  let c = transpose b in
  map  (\(row: []i32) ->
         map  (*2) row) c
