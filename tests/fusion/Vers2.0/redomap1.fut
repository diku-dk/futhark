-- ==
-- input {
--   [1.0f32,-4.0f32,-2.4f32]
-- }
-- output {
--   -5.4f32
--   [2.0f32, -3.0f32, -1.4f32]
--   [3.0f32, -2.0f32, -0.4f32]
--   [4.0f32, -6.0f32, -2.8f32]
--   [9.0f32, -6.0f32, -1.2f32]
-- }
-- structure {
--      Screma 1
-- }
--

def main (arr: []f32) : (f32, []f32, []f32, []f32, []f32) =
  let xy = map (\(a: f32) : (f32, f32) -> (a + 1.0, a + 2.0)) arr
  let (x, y) = unzip (xy)
  let z = map (* 2.0) x
  let w = map (* 3.0) y
  let r = reduce (+) (0.0) arr
  in (r, x, y, z, w)
