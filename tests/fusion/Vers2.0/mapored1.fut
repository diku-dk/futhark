-- ==
-- input {
--   [1.0f32,-4.0f32,-2.4f32]
-- }
-- output {
--   3.6f32
--   [12.0f32, -3.0f32, 1.8000000000000003f32]
--   [40.0f32, 15.0f32, 23.0f32]
--   [0.7f32, -2.8f32, -1.68f32]
-- }
-- structure {
--      Screma 1
-- }
--
def main (arr: []f32) : (f32, []f32, []f32, []f32) =
  let a = map (+ 3.0) arr
  let b = map (+ 7.0) arr
  let s = reduce (+) (0.0) a
  let x1 = map (* 3.0) a
  let x2 = map (* 5.0) b
  let x3 = map (* 0.7) arr
  in (s, x1, x2, x3)
