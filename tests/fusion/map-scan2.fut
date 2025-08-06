-- ==
-- input {
--   [1,2,3,4,5,6,7]
-- }
-- output {
--   [-1, -1, 0, 2, 5, 9, 14]
-- }
-- structure {
--   Screma 1
-- }
def main (a: []i32) : []i32 =
  let (_, b) = unzip (map (\(x: i32) : (i32, i32) -> (x + 2, x - 2)) a)
  let c = scan (+) 0 b
  in c
