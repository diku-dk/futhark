-- ==
-- input {
--   [1.0f32,-4.0f32,-2.4f32]
-- }
-- output {
--   129.6f32
--   [1.0f32,-3.0f32, -5.4f32]
--   [2.0f32,-6.0f32,-10.8f32]
--   [2.0f32,-4.0f32,-14.8f32]
--   [7.0f32, 1.0f32, -9.8f32]
-- }
-- structure {
--    Screma 2
-- }
--
def main (arr: []f32) : (f32, []f32, []f32, []f32, []f32) =
  let sa = scan (+) (0.0) arr
  let b = map (* 2.0) sa
  let sb = scan (+) (0.0) b
  let c = map (+ 5.0) sb
  let r = reduce (*) (1.0) b
  in (r, sa, b, sb, c)
