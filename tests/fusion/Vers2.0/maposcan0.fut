-- ==
-- input {
--   [1.0f32,-4.0f32,-2.4f32]
-- }
-- output {
--   [1.0f32,-3.0f32,-5.4f32]
--   [2.0f32,-6.0f32,-10.8f32]
-- }
-- structure {
--    Screma 1
-- }
def main (arr: []f32) : ([]f32, []f32) =
  let sa = scan (+) (0.0) arr
  let b = map (* 2.0) sa
  in (sa, b)
