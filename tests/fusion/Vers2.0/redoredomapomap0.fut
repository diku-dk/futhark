-- ==
-- tags { no_webgpu }
-- input {
--   [1.0,-4.0,-2.4]
-- }
-- output {
--   -5.4
--   [2.0,-3.0,-1.4]
--   8.4
--   [4.0,-6.0,-2.8]
--   3.0
--   [0.0,1.0,2.0]
-- }
-- structure {
--      /Screma 2
-- }
--
def mul2 (x: []f64) (i: i32) : f64 = x[i] * 2.0

def main [n] (arr: [n]f64) : (f64, []f64, f64, []f64, f64, []f64) =
  let r1 = reduce (+) (0.0) arr
  let x = map (+ 1.0) arr
  let r2 = reduce (*) (1.0) x
  let y = map (mul2 (x)) (map i32.i64 (iota (n)))
  let z = map f64.i64 (iota (n))
  let r3 = reduce (+) (0.0) z
  in (r1, x, r2, y, r3, z)
