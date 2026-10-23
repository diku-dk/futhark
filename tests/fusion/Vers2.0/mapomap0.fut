-- ==
-- input {
--   [1.0f32,-4.0f32,-2.4f32]
-- }
-- output {
--   [6.0f32, 1.0f32, 2.6f32]
--   [2.0f32, -3.0f32, -1.4f32]
--   [3.0f32, -7.0f32, -3.8f32]
-- }
-- structure {
--      Screma 1
-- }
--
def main (arr: []f32) : ([]f32, []f32, []f32) =
  let x = map (+ 1.0) arr
  let y = map2 (+) x arr
  let r = map (+ 5.0) arr
  in (r, x, y)
