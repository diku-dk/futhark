-- ==
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   [1.0,-3.0,-5.4]
--   [2.0,-6.0,-10.8]
-- }
-- structure {
--    /Stream 1
--    /Screma 0
-- }
def main (arr: []f64) : ([]f64, []f64) =
  let sa = scan (+) (0.0) arr
  let b = map (* 2.0) sa
  in (sa, b)
