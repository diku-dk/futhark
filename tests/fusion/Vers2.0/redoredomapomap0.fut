-- ==
-- input {
--   [1.0f32,-4.0f32,-2.4f32]
-- }
-- output {
--   -5.4f32
--   [2.0f32,-3.0f32,-1.4f32]
--   8.4f32
--   [4.0f32,-6.0f32,-2.8f32]
--   3.0f32
--   [0.0f32,1.0f32,2.0f32]
-- }
-- structure {
--      /Screma 2
-- }
--
def mul2 (x: []f32) (i: i32) : f32 = x[i] * 2.0

def main [n] (arr: [n]f32) : (f32, []f32, f32, []f32, f32, []f32) =
  let r1 = reduce (+) (0.0) arr
  let x = map (+ 1.0) arr
  let r2 = reduce (*) (1.0) x
  let y = map (mul2 (x)) (map i32.i64 (iota (n)))
  let z = map f32.i64 (iota (n))
  let r3 = reduce (+) (0.0) z
  in (r1, x, r2, y, r3, z)
