-- ==
-- input {
--   [1.0f32,-4.0f32,-2.4f32]
-- }
-- output {
--   -5.4f32
--   [2.0f32,-3.0f32,-1.4f32]
-- }
-- structure {
--      Screma 1
-- }
--
def main (arr: []f32) : (f32, []f32) =
  let r = reduce (+) (0.0) arr
  let x = map (+ 1.0) arr
  in (r, x)
