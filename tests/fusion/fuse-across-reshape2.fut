-- ==
-- input {
-- }
-- output {
--   [[0, 9, 18], [27, 36, 45], [54, 63, 72]]
-- }
def main: [][]i32 =
  let n = 9
  let a = map (\i -> replicate n (i32.i64 i))
              (iota n)
  let b = unflatten_3d 3 3 9 (flatten a) in
  map  (\(row: [][]i32) ->
         map  (\(x: []i32): i32  -> reduce (+) 0 x) row) b
