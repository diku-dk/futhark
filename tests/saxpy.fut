-- Single-Precision A·X Plus Y
--
-- ==
-- input {
--   2.0f32
--   [1.0f32,2.0f32,3.0f32]
--   [4.0f32,5.0f32,6.0f32]
-- }
-- output {
--   [6.0f32, 9.0f32, 12.0f32]
-- }

def main (a: f32) (x: []f32) (y: []f32) : []f32 =
  map2 (+) (map (a *) x) y
