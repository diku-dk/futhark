-- ==
-- input {
--   [[1,2,3],[4,5,6],[7,8,9]]
-- }
-- output {
--   [[0, 1, 2], [0, 2, 4], [0, 3, 6]]
-- }
let main [n][m] (a: [n][m]i32): [][]i32 =
  let foo = replicate m (iota n)
  let bar = replicate m (iota n)
  let b = replicate n (iota m)
  let c = map (\(xs: []i32, ys: []i32,zs: []i32) ->
                map (\(x: i32, y: i32, z: i32): i32  -> x+y*z) (zip3 xs ys zs)) (
              zip3 foo bar (transpose b)) in
  c
