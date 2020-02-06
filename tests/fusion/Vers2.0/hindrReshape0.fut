-- ==
-- input {
-- }
-- output {
--   [1, 2, 3, 4, 5, 6, 7, 8, 9]
--   [[2, 4, 6], [8, 10, 12], [14, 16, 18]]
-- }
let main: ([]i32,[][]i32) =
  let n = 9
  let a = map (+1) (iota(n))
  let b = unflatten 3 3 a
  let c = map  (\(row: []i32) ->
                    map  (\(x: i32): i32  -> x*2) row
              ) b
  in (a,c)
