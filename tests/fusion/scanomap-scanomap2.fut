-- ==
-- input {
--   [1,2,3,4,5,6,7]
-- }
-- output {
--   [2, 3, 4, 5, 6, 7, 8]
--   [2, 5, 9, 14, 20, 27, 35]
--   [3, 4, 5, 6, 7, 8, 9]
--   [3, 12, 60, 360,2520,20160,181440]
-- }
-- structure {
--   Screma 1
-- }
def main (inp: []i32) : ([]i32, []i32, []i32, []i32) =
  let a = map (+ 1) inp
  let b = scan (+) 0 a
  let c = map (+ 1) a
  let d = scan (*) 1 c
  in (a, b, c, d)
