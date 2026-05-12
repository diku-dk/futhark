-- ==
-- input {
--   [[1,2,3],[4,5,6],[7,8,9]]
-- }
-- output {
--   [[3, 4, 5], [7, 9, 11], [14, 17, 20]]
-- }
-- structure { /Screma 1 }
def main (input: [][3]i32) : [][]i32 =
  let x =
    scan (\(a: []i32) (b: []i32) : [3]i32 ->
            map2 (+) a b)
         (replicate 3 0)
         input
  in map (\(r: []i32) : [3]i32 ->
            map (+ 2) r)
         x
