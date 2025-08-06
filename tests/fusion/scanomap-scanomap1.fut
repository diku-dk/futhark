-- ==
-- input {
--   [1,2,3,4,5,6,7]
-- }
-- output {
--   [2, 3, 4, 5, 6, 7, 8]
--   [2, 5, 9, 14, 20, 27, 35]
--   [2, 6, 24, 120, 720, 5040, 40320]
-- }
-- structure {
--   Screma 1
-- }
def main (inp: []i32) : ([]i32, []i32, []i32) =
  let a = map (+ 1) inp
  let b = scan (+) 0 a
  let c = scan (*) 1 a
  in (a, b, c)
