-- ==
-- input {
--   [ [1,2,3], [4,5,6]
--   , [6,7,8], [9,10,11]
--   ]
--   [1,2,3,4]
--   5
-- }
-- output {
--   [[7, 8, 9],
--    [16, 17, 18],
--    [24, 25, 26],
--    [33, 34, 35]]
-- }
let addToRow [n] (xs: [n]i32, y: i32): [n]i32 =
  map (\(x: i32): i32  -> x+y) xs

let main (xss: [][]i32) (cs: []i32) (y: i32): [][]i32 =
  map  (\(xs: []i32, c: i32)  ->
         let y' = y * c + c
         let zs = addToRow(xs,y') in
         zs
      ) (zip  xss cs)
