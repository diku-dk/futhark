-- ==
-- input {
-- }
-- output {
--   [[0, 9, 18], [27, 36, 45], [54, 63, 72]]
-- }
def main: [][]i32 =
  let a = map (\i -> replicate 9 (i32.i64 i))
              (iota (3*3))
  let b = unflatten_3d (flatten a) in
  map  (\(row: [][]i32) ->
         map  (\(x: []i32): i32  -> reduce (+) 0 x) row) b
